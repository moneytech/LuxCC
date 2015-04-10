#include "vm_cgen.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "../as_lk_vm/vm.h"
#include "../util.h"
#include "../decl.h"
#include "../expr.h"
#include "../decl.h"
#include "../arena.h"
#include "../imp_lim.h"
#include "../error.h"

#define HASH_SIZE       4093
#define HASH_VAL(s)     (hash(s)%HASH_SIZE)
#define HASH_VAL2(x)    (hash2(x)%HASH_SIZE)

static int curr_scope = 0;
static char *curr_func; /* name of current function */

typedef struct Location Location;
struct Location {
    char *id;
    int offset;
    Location *next;
} *location_table[MAX_NEST][HASH_SIZE]; /* location_table[0] unused */
static Arena *location_arena[MAX_NEST];
static void init_location_arena(void)
{
    int i;

    for (i = 0; i < MAX_NEST; i++)
        location_arena[i] = arena_new(8192);
}
static Location *alloc_location(void)
{
    void *p;

    if ((p=arena_alloc(location_arena[curr_scope], sizeof(Location))) == NULL)
        my_assert(0, "alloc_location()");

    return p;
}
static void cgen_pop_scope(void)
{
    memset(&location_table[curr_scope][0], 0, sizeof(Location *)*HASH_SIZE);
    arena_reset(location_arena[curr_scope]);
    --curr_scope;
}
static void cgen_push_scope(void)
{
    ++curr_scope;
}
Location *lookup_location(char *id)
{
    int n;
    unsigned h;
    Location *np;

    h = HASH_VAL(id);
    for (n = curr_scope; n >= 0; n--)
        for (np = location_table[n][h]; np != NULL; np = np->next)
            if (equal(id, np->id))
                return np;

    my_assert(0, "lookup_location()");
}
static Location *new_location(char *id, int offset)
{
    unsigned h;
    Location *np;

    h = HASH_VAL(id);
    /*for (np = location_table[curr_scope][h]; np != NULL; np = np->next)
        if (equal(id, np->id))
            break;*/

    np = alloc_location();
    np->id = id;
    np->offset = offset;
    np->next = location_table[curr_scope][h];
    location_table[curr_scope][h] = np;

    return np;
}

static void compound_statement(ExecNode *s, int push_scope);
static void if_statement(ExecNode *s);
static void switch_statement(ExecNode *s);
static void while_statement(ExecNode *s);
static void do_statement(ExecNode *s);
static void for_statement(ExecNode *s);
static void goto_statement(ExecNode *s);
static void continue_statement(void);
static void break_statement(void);
static void return_statement(ExecNode *s);
static void case_statement(ExecNode *s);
static void default_statement(ExecNode *s);
static void expression_statement(ExecNode *s);
static void label_statement(ExecNode *s);
static void statement(ExecNode *s);

static void expression(ExecNode *e, int is_addr);
static void expr_convert(ExecNode *e, Declaration *dest);

// TODO: make this more flexible using a growable buffer.
#define OUT_BUF_SIZE    32768
static char output_buffer[OUT_BUF_SIZE];
static char *buf_curr = output_buffer;
#define emit(...) ( buf_curr+=sprintf(buf_curr, __VA_ARGS__), buf_curr+=sprintf(buf_curr, "\n") )
#define flush_output_buffer() ( fprintf(stdout, "%s", output_buffer), buf_curr=output_buffer )

static void function_definition(TypeExp *decl_specs, TypeExp *header);
static void static_object_definition(TypeExp *decl_specs, TypeExp *declarator, int mangle_name);

static void load(ExecNode *e);
static void load_addr(ExecNode *e);
static void store(Declaration *dest_ty);

/*
 * String literals.
 */
static char *string_literal_pool[512];
static unsigned str_lit_count;

static unsigned new_string_literal(char *s)
{
    /* TODO: search into the pool before add a new string */

    string_literal_pool[str_lit_count] = s;

    return str_lit_count++;
}

static void emit_string_literals(void)
{
    unsigned n;
    unsigned char *c;

    emit(".data");
    for (n = 0; n < str_lit_count; n++) {
        emit("@S%u:", n);
        c = (unsigned char *)string_literal_pool[n];
        do
            emit(".byte %u", *c);
        while (*c++ != '\0');
    }
    flush_output_buffer();
}

void vm_cgen(void)
{
    ExternId *ed;

    init_location_arena();

    /*emit(".text");
    emit("ldi main;");
    emit("call 0;");
    emit("halt;");*/

    for (ed = get_extern_symtab(); ed != NULL; ed = ed->next) {
        if (ed->status == REFERENCED) {
            TypeExp *scs;

            if ((scs=get_sto_class_spec(ed->decl_specs))==NULL || scs->op!=TOK_STATIC)
                emit(".extern %s", ed->declarator->str);
        } else {
            if (ed->declarator->child!=NULL && ed->declarator->child->op==TOK_FUNCTION)
                function_definition(ed->decl_specs, ed->declarator);
            else
                static_object_definition(ed->decl_specs, ed->declarator, FALSE);
        }

        flush_output_buffer();
    }

    emit_string_literals();
}

/*
 * The amount of space to allocate for the
 * current function's local variables.
 */
static int size_of_local_area = 0;
/*
 * Used to compute the addresses of local variables.
 */
static int local_offset = VM_LOCAL_START;

/*
 * Return type of the current function being processed.
 * Used for return statement.
 */
static Declaration ret_ty;

void function_definition(TypeExp *decl_specs, TypeExp *header)
{
    DeclList *p;
    TypeExp *scs;
    int param_offs;
    char *addsp_param, num[16];

    curr_func = header->str;
    emit("# ==== start of definition of function `%s' ====", curr_func);
    emit(".text");
    emit("%s:", curr_func);
    if ((scs=get_sto_class_spec(decl_specs))==NULL || scs->op!=TOK_STATIC)
        emit(".global %s", curr_func);

    addsp_param = buf_curr+strlen("addsp")+1; /* for later fix up */
    emit("addsp \x20\x20\x20\x20"); /* allow at most 9999 bytes of local storage */

    cgen_push_scope();

    p = header->child->attr.dl;
    if (get_type_spec(p->decl->decl_specs)->op==TOK_VOID && p->decl->idl==NULL)
        p = NULL; /* function with no parameters */

    param_offs = VM_LOCAL_PARAM_END;
    while (p != NULL) {
        Declaration pty;

        if (p->decl->idl!=NULL && p->decl->idl->op==TOK_ELLIPSIS)
            break; /* start of optional parameters (`...') */

        pty.decl_specs = p->decl->decl_specs;
        pty.idl = p->decl->idl->child;
        param_offs -= round_up(compute_sizeof(&pty), VM_STACK_ALIGN);
        new_location(p->decl->idl->str, param_offs);
        emit("# param:`%s', offset:%d", p->decl->idl->str, param_offs);

        p = p->next;
    }

    /* set return type before encounter any return statement */
    ret_ty.decl_specs = decl_specs;
    ret_ty.idl = header->child->child;

    compound_statement(header->attr.e, FALSE);
    cgen_pop_scope();

    /* fix up the amount of storage to allocate for locals */
    sprintf(num, "%d", round_up(size_of_local_area-VM_LOCAL_START, VM_STACK_ALIGN));
    strncpy(addsp_param, num, strlen(num));
    addsp_param[strlen(num)] = ';';

    size_of_local_area = 0;
    local_offset = VM_LOCAL_START;

    emit("ldi 0;");
    emit("ret;");
}

void do_static_init(TypeExp *ds, TypeExp *dct, ExecNode *e)
{
    TypeExp *ts;

    if (dct != NULL) {
        unsigned nelem;

        if (dct->op != TOK_SUBSCRIPT)
            goto scalar; /* pointer */

        /*
         * Array.
         */
        nelem = dct->attr.e->attr.uval;
        if (e->kind.exp == StrLitExp) {
            /*
             * Character array initialized by string literal.
             */
            unsigned n;
            unsigned char *c;

            n = 0;
            c = (unsigned char *)e->attr.str;
            do
                emit(".byte %u", *c), ++n;
            while (n<nelem && *c++!='\0');

            /* zero any trailing elements */
            if (n < nelem)
                emit(".zero %u", nelem-n);
        } else {
            /*
             * Handle elements with explicit initializer.
             */
            e = e->child[0];
            for (; e!=NULL && nelem!=0; e=e->sibling, --nelem)
                do_static_init(ds, dct->child, e);

            /*
             * Handle elements without explicit initializer.
             */
            if (nelem != 0) {
                Declaration elem_ty;

                elem_ty.decl_specs = ds;
                elem_ty.idl = dct->child;
                emit(".align %u", get_alignment(&elem_ty));
                emit(".zero %u", nelem*compute_sizeof(&elem_ty));
            }
        }
    } else if ((ts=get_type_spec(ds))->op == TOK_STRUCT) {
        /*
         * Struct.
         */
        DeclList *d;
        int full_init;

        e = e->child[0];

        /*
         * Handle members with explicit initializer.
         */
        d = ts->attr.dl;
        full_init = FALSE;
        for (; d != NULL; d = d->next) {
            dct = d->decl->idl;
            for (; e!=NULL && dct!=NULL; e=e->sibling, dct=dct->sibling)
                do_static_init(d->decl->decl_specs, dct->child, e);

            if (e == NULL) {
                if (dct==NULL && d->next==NULL)
                    full_init = TRUE;
                break;
            }
        }

        /*
         * Handle members without explicit initializer.
         */
        if (!full_init) {
            if (dct == NULL) {
                d = d->next;
                dct = d->decl->idl;
            }
            while (TRUE) {
                while (dct != NULL) {
                    Declaration ty;

                    ty.decl_specs = d->decl->decl_specs;
                    ty.idl = dct->child;
                    emit(".align %u", get_alignment(&ty));
                    emit(".zero %u", compute_sizeof(&ty));

                    dct = dct->sibling;
                }
                d = d->next;
                if (d != NULL)
                    dct = d->decl->idl;
                else
                    break;
            }
        }
    } else if (ts->op == TOK_UNION) {
        /*
         * Union.
         */
        e = e->child[0];

        /* initialize the first named member */
        do_static_init(ts->attr.dl->decl->decl_specs, ts->attr.dl->decl->idl->child, e);
    } else {
        /*
         * Scalar.
         */
        Declaration dest_ty;
scalar:
        dest_ty.decl_specs = ds;
        dest_ty.idl = dct;
        switch (get_type_category(&dest_ty)) {
        case TOK_CHAR: case TOK_UNSIGNED_CHAR: case TOK_SIGNED_CHAR:
            if (is_integer(get_type_category(&e->type))) {
                emit(".byte %lu", eval_int_const_expr(e));
            } else {
                emit(".byte 0");
                goto bad_init;
            }
            break;
        case TOK_SHORT: case TOK_UNSIGNED_SHORT:
            emit(".align 2");
            if (is_integer(get_type_category(&e->type))) {
                emit(".word %lu", eval_int_const_expr(e));
            } else {
                emit(".word 0");
                goto bad_init;
            }
            break;
        case TOK_INT: case TOK_UNSIGNED:
        case TOK_LONG: case TOK_UNSIGNED_LONG:
        case TOK_ENUM:
        case TOK_STAR:
            emit(".align 4");
            if (is_integer(get_type_category(&e->type))) {
                emit(".dword %lu", eval_int_const_expr(e));
            } else if (e->kind.exp == StrLitExp) {
                emit(".dword @S%u", new_string_literal(e->attr.str));
            } else {
                emit(".dword 0");
                goto bad_init;
            }
            break;
        }
        return; /* OK */
bad_init:
        emit_warning(e->info->src_file, e->info->src_line, e->info->src_column,
        "initializer form not supported, defaulting to zero");
    }
}

void static_object_definition(TypeExp *decl_specs, TypeExp *declarator, int mangle_name)
{
    TypeExp *scs;
    Declaration ty;
    ExecNode *initializer;
    unsigned alignment;

    ty.decl_specs = decl_specs;
    ty.idl = declarator->child;
    initializer = declarator->attr.e;

    /* segment */
    if (initializer != NULL)
        emit(".data");
    else
        emit(".bss");

    /* alignment (only for .bss) */
    if (initializer==NULL && (alignment=get_alignment(&ty))>1)
        emit(".align %u", alignment);

    /* label */
    if (mangle_name)
        /* mangled name = '@' + current function name + '_' + static local object name */
        emit("@%s_%s:", curr_func, declarator->str);
    else
        emit("%s:", declarator->str);

    /* allocation/initialization */
    if (initializer != NULL)
        do_static_init(ty.decl_specs, ty.idl, initializer);
    else
        emit(".res %u", compute_sizeof(&ty));

    if ((scs=get_sto_class_spec(decl_specs))==NULL || scs->op!=TOK_STATIC)
        emit(".global %s", declarator->str);
}

// =============================================================================
// Statements
// =============================================================================

static unsigned btarget_stack[128], ctarget_stack[128];
static int bt_stack_top = -1, ct_stack_top = -1;

static
void push_break_target(unsigned lab)
{
    btarget_stack[++bt_stack_top] = lab;
}

static
void pop_break_target(void)
{
    --bt_stack_top;
}

static
void push_continue_target(unsigned lab)
{
    ctarget_stack[++ct_stack_top] = lab;
}

static
void pop_continue_target(void)
{
    --ct_stack_top;
}

void statement(ExecNode *s)
{
    switch (s->kind.stmt) {
    case CmpndStmt:
        compound_statement(s, TRUE);
        break;
    case IfStmt:
        if_statement(s);
        break;
    case SwitchStmt:
        switch_statement(s);
        break;
    case WhileStmt:
        while_statement(s);
        break;
    case DoStmt:
        do_statement(s);
        break;
    case ForStmt:
        for_statement(s);
        break;
    case GotoStmt:
        goto_statement(s);
        break;
    case ContinueStmt:
        continue_statement();
        break;
    case BreakStmt:
        break_statement();
        break;
    case ReturnStmt:
        return_statement(s);
        break;
    case CaseStmt:
        case_statement(s);
        break;
    case DefaultStmt:
        default_statement(s);
        break;
    case ExpStmt:
        expression_statement(s);
        break;
    case LabelStmt:
        label_statement(s);
        break;
    }
}

void do_auto_init(TypeExp *ds, TypeExp *dct, ExecNode *e, int offset)
{
    TypeExp *ts;

    if (dct != NULL) {
        unsigned nelem;

        if (dct->op != TOK_SUBSCRIPT)
            goto scalar; /* pointer */

        /*
         * Array.
         */
        nelem = dct->attr.e->attr.uval;
        if (e->kind.exp == StrLitExp) {
            /*
             * Character array initialized by string literal.
             */
            unsigned n;

            emit("ldbp %u;", offset);
            expression(e, FALSE);
            n = strlen(e->attr.str)+1;
            if (nelem == n) {
                /* fits nicely */
                emit("memcpy %u;", n);
            } else if (nelem < n) {
                /* no enough room; just copy the first nelem chars of the string */
                emit("memcpy %u;", nelem);
            } else {
                /* copy all the string and zero the trailing elements */
                emit("memcpy %u;", n);
                emit("ldi %u;", n);
                emit("add;");
                emit("ldi 0;");
                emit("fill %u;", nelem-n);
            }
            emit("pop;");
        } else {
            unsigned elem_size;
            Declaration elem_ty;

            /*
             * Get element size.
             */
            elem_ty.decl_specs = ds;
            elem_ty.idl = dct->child;
            elem_size = compute_sizeof(&elem_ty);

            /*
             * Handle elements with explicit initializer.
             */
            e = e->child[0];
            for (; e!=NULL && nelem!=0; e=e->sibling, --nelem) {
                do_auto_init(ds, dct->child, e, offset);
                offset += elem_size;
            }

            /*
             * Handle elements without explicit initializer.
             */
            if (nelem != 0) {
                /* there are nelem elements to zero */
                emit("ldbp %u;", offset);
                emit("ldi 0;");
                emit("fill %u;", nelem*elem_size);
                emit("pop;");
            }
        }
    } else if ((ts=get_type_spec(ds))->op == TOK_STRUCT) {
        /*
         * Struct.
         */
        DeclList *d;
        int full_init;


        if (e->attr.op != TOK_INIT_LIST)
            goto scalar;
        e = e->child[0];

        /*
         * Handle members with explicit initializer.
         */
        d = ts->attr.dl;
        full_init = FALSE;
        for (; d != NULL; d = d->next) {
            dct = d->decl->idl;
            for (; e!=NULL && dct!=NULL; e=e->sibling, dct=dct->sibling) {
                unsigned mem_offs;

                mem_offs = get_member_descriptor(ts, dct->str)->offset;
                do_auto_init(d->decl->decl_specs, dct->child, e, offset+mem_offs);
            }

            if (e == NULL) {
                if (dct==NULL && d->next==NULL)
                    full_init = TRUE;
                break;
            }
        }

        /*
         * Handle members without explicit initializer.
         */
        if (!full_init) {
            if (dct == NULL) {
                d = d->next;
                dct = d->decl->idl;
            }
            while (TRUE) {
                while (dct != NULL) {
                    StructMember *md;

                    md = get_member_descriptor(ts, dct->str);
                    emit("ldbp %u;", offset+md->offset);
                    emit("ldi 0;");
                    emit("fill %u;", md->size);
                    emit("pop;");
                    dct = dct->sibling;
                }
                d = d->next;
                if (d != NULL)
                    dct = d->decl->idl;
                else
                    break;
            }
        }
    } else if (ts->op == TOK_UNION) {
        /*
         * Union.
         */

        if (e->attr.op != TOK_INIT_LIST)
            goto scalar;
        e = e->child[0];

        /* initialize the first named member */
        do_auto_init(ts->attr.dl->decl->decl_specs, ts->attr.dl->decl->idl->child, e, offset);
    } else {
        /*
         * Scalar.
         */
        Declaration dest_ty;
scalar:
        dest_ty.decl_specs = ds;
        dest_ty.idl = dct;
        expr_convert(e, &dest_ty);
        emit("ldbp %u;", offset);
        store(&dest_ty);
        emit("pop;");
    }
}

void compound_statement(ExecNode *s, int push_scope)
{
    ExecNode *sl;
    int old_local_offset;

    if (s->locals != NULL) {
        DeclList *dl;

        old_local_offset = local_offset;
        if (push_scope)
            cgen_push_scope();

        /* traverse declaration list */
        for (dl = s->locals; dl != NULL; dl = dl->next) {
            TypeExp *dct, *scs;

            /* check for extern/static local variables */
            if ((scs=get_sto_class_spec(dl->decl->decl_specs)) != NULL) {
                if (scs->op == TOK_STATIC) {
                    for (dct = dl->decl->idl; dct != NULL; dct = dct->sibling)
                        static_object_definition(dl->decl->decl_specs, dct, TRUE);
                    emit(".text");
                    continue;
                } else if (scs->op == TOK_EXTERN) {
                    emit(".extern %s", dl->decl->idl->str);
                    continue;
                } else if (scs->op == TOK_TYPEDEF) {
                    continue;
                }
            }

            /* traverse init declarator list */
            for (dct = dl->decl->idl; dct != NULL; dct = dct->sibling) {
                Declaration lty;

                lty.decl_specs = dl->decl->decl_specs;
                lty.idl = dct->child;
                local_offset = round_up(local_offset, get_alignment(&lty));
                new_location(dct->str, local_offset);
                emit("# var: %s, offset: %d", dct->str, local_offset);
                if (dct->attr.e != NULL)
                    do_auto_init(lty.decl_specs, lty.idl, dct->attr.e, local_offset);
                local_offset += compute_sizeof(&lty);
            }
        }
    }
    for (sl = s->child[0]; sl != NULL; sl = sl->sibling)
        statement(sl);

    if (local_offset > size_of_local_area)
        size_of_local_area = local_offset;

    if (push_scope && s->locals!=NULL) {
        local_offset = old_local_offset;
        cgen_pop_scope();
    }
}

/* some shorthand macros */
#define emit_lab(n)         (emit("@L%u:", n))
#define emit_jmp(target)    (emit("jmp @L%u;", target))
#define emit_jmpf(target)   (emit("jmpf @L%u;", target))
#define emit_jmpt(target)   (emit("jmpt @L%u;", target))

static unsigned new_label(void);
unsigned new_label(void)
{
    static unsigned label_count = 1;
    return label_count++;
}

void if_statement(ExecNode *s)
{
    /*
     * if (e)
     *      stmt1
     * else
     *      stmt2
     */

    unsigned L1, L2;

    /* e */
    expression(s->child[0], FALSE);
    L1 = L2 = new_label();
    emit_jmpf(L1);
    /* stmt1 */
    statement(s->child[1]);
    if (s->child[2] != NULL) {
        /* stmt2 */
        L2 = new_label();
        emit_jmp(L2);
        emit_lab(L1);
        statement(s->child[2]);
    }
    emit_lab(L2); /* L1==L2 if no ELSE-part */
}

void while_statement(ExecNode *s)
{
    /*
     * while (e)
     *      stmt
     */

    unsigned L1, L2;

    L1 = new_label();
    L2 = new_label();
    /* e */
    emit_lab(L1);
    expression(s->child[0], FALSE);
    emit_jmpf(L2);
    /* stmt */
    push_break_target(L2), push_continue_target(L1);
    statement(s->child[1]);
    pop_break_target(), pop_continue_target();
    emit_jmp(L1);
    emit_lab(L2);
}

void do_statement(ExecNode *s)
{
    /*
     * do
     *      stmt
     * while (e);
     */

    unsigned L1, L2, L3;

    L1 = new_label();
    L2 = new_label();
    L3 = new_label();
    /* stmt */
    emit_lab(L1);
    push_break_target(L3), push_continue_target(L2);
    statement(s->child[1]);
    pop_break_target(), pop_continue_target();
    /* e */
    emit_lab(L2);
    expression(s->child[0], FALSE);
    emit_jmpf(L3);
    emit_jmp(L1);
    emit_lab(L3);
}

void for_statement(ExecNode *s)
{
    /*
     * for (e1; e2; e3)
     *      stmt;
     */

    unsigned L1, L2, L3;

    /* e1 */
    if (s->child[1] != NULL) {
        expression(s->child[1], FALSE);
        emit("pop;");
    }

    L1 = new_label();
    if (s->child[2] != NULL)
        L2 = new_label();
    L3 = new_label();

    /* e2 */
    emit_lab(L1);
    if (s->child[0] != NULL) {
        expression(s->child[0], FALSE);
        emit_jmpf(L3);
    }
    /* stmt */
    push_break_target(L3), push_continue_target((s->child[2]!=NULL)?L2:L1);
    statement(s->child[3]);
    pop_break_target(), pop_continue_target();
    /* e3 */
    if (s->child[2] != NULL) {
        emit_lab(L2);
        expression(s->child[2], FALSE);
        emit("pop;");
    }
    emit_jmp(L1);
    emit_lab(L3);
}

void goto_statement(ExecNode *s)
{
    emit("jmp @@%s_%s;", curr_func, s->attr.str);
}

void label_statement(ExecNode *s)
{
    /* mangled label name = "@@" + current function name + '_' + label name */
    emit("@@%s_%s:", curr_func, s->attr.str);
    statement(s->child[0]);
}

void continue_statement(void)
{
    emit_jmp(ctarget_stack[ct_stack_top]);
}

void break_statement(void)
{
    emit_jmp(btarget_stack[bt_stack_top]);
}

void return_statement(ExecNode *s)
{
    if (s->child[0] != NULL)
        expr_convert(s->child[0], &ret_ty);
    else
        emit("ldi 0;");
    emit("ret;");
}

void expression_statement(ExecNode *s)
{
    if (s->child[0] == NULL)
        return;

    expression(s->child[0], FALSE);
    /*if (get_type_category(&s->child[0]->type) != TOK_VOID)*/
        emit("pop;");
}

/*
 * Switch statement.
 */

typedef struct SwitchLabel SwitchLabel;
static struct SwitchLabel {
    unsigned lab;
    int val, is_default;
    SwitchLabel *next;
} *switch_labels[MAX_SWITCH_NEST][HASH_SIZE];

static int switch_nesting_level = -1;

static
int cmp_switch_label(const void *p1, const void *p2)
{
    SwitchLabel *x1 = *(SwitchLabel **)p1;
    SwitchLabel *x2 = *(SwitchLabel **)p2;

    if (x1->is_default)
        return -1;
    if (x2->is_default)
        return 1;

    if (x1->val < x2->val)
        return -1;
    else if (x1->val == x2->val)
        return 0;
    else
        return 1;
}

static
void install_switch_label(int val, int is_default, unsigned lab)
{
    unsigned h;
    SwitchLabel *np;

    np = malloc(sizeof(SwitchLabel));
    np->lab = lab;
    np->val = val;
    np->is_default = is_default;
    h = is_default?0:HASH_VAL2((unsigned long)val);
    np->next = switch_labels[switch_nesting_level][h];
    switch_labels[switch_nesting_level][h] = np;
}

void switch_statement(ExecNode *s)
{
    int i, st_size;
    unsigned ST, EXIT;
    SwitchLabel *search_table[128], *np;

    /*
     * Controlling expression.
     */
    ST = new_label();
    expression(s->child[0], FALSE);
    emit("ldi @T%u;", ST);
    emit("switch;");

    /*
     * Body.
     */
    ++switch_nesting_level;
    EXIT = new_label();
    push_break_target(EXIT);
    statement(s->child[1]);
    pop_break_target();
    emit_lab(EXIT);

    /*
     * Build search table.
     */
    st_size = 0;
    for (i = 0; i < HASH_SIZE; i++) {
        if (switch_labels[switch_nesting_level][i] != NULL) {
            for (np = switch_labels[switch_nesting_level][i]; np != NULL; np = np->next)
                search_table[st_size++] = np;
            switch_labels[switch_nesting_level][i] = NULL;
        }
    }
    --switch_nesting_level;
    if (st_size != 0)
        qsort(search_table, st_size, sizeof(search_table[0]), cmp_switch_label);

    /*
     * Emit search table.
     */
    emit(".data");
    emit(".align 4");
    emit("@T%u:", ST);
    if (st_size == 0) {
        /* if there are no labels at all, the body of the switch is skipped */
        emit(".dword 1");
        emit(".dword @L%u", EXIT);
        emit(".text");
        return;
    }

    /* emit case values */
    /* the first value corresponds to the default case and is the size of the search table */
    if (!search_table[0]->is_default) {
        /* there is no default label */
        emit(".dword %u", st_size+1);
        i = 0;
    } else {
        emit(".dword %u", st_size);
        i = 1;
    }
    while (i < st_size) {
        emit(".dword %u", search_table[i]->val);
        ++i;
    }

    /* emit labels */
    /* the first label correspond to the default case; if there is none the exit label acts as default */
    if (!search_table[0]->is_default)
        emit(".dword @L%u", EXIT);
    for (i = 0; i < st_size; i++) {
        emit(".dword @L%u", search_table[i]->lab);
        free(search_table[i]);
    }
    emit(".text");
}

void case_statement(ExecNode *s)
{
    unsigned L;

    L = new_label();
    install_switch_label(s->child[0]->attr.val, FALSE, L);
    emit_lab(L);
    statement(s->child[1]);
}

void default_statement(ExecNode *s)
{
    unsigned L;

    L = new_label();
    install_switch_label(0, TRUE, L);
    emit_lab(L);
    statement(s->child[0]);
}

// =============================================================================
// Expressions
// =============================================================================

/*
 * Generate code for expression `e'.
 * Cast the result (dword on top of the stack) to type `dest'.
 */
void expr_convert(ExecNode *e, Declaration *dest)
{
    Token cat_dest, cat_src;

    expression(e, FALSE);

    cat_src  = get_type_category(&e->type);
    cat_dest = get_type_category(dest);

    switch(cat_dest) {
    case TOK_CHAR:
    case TOK_SIGNED_CHAR:
        if (cat_src!=TOK_CHAR && cat_src!=TOK_SIGNED_CHAR)
            emit("dw2b;");
        break;
    case TOK_UNSIGNED_CHAR:
        if (cat_src != TOK_UNSIGNED_CHAR)
            emit("dw2ub;");
        break;
    case TOK_SHORT:
        if (cat_src != TOK_SHORT)
            emit("dw2w;");
        break;
    case TOK_UNSIGNED_SHORT:
        if (cat_src != TOK_UNSIGNED_SHORT)
            emit("dw2uw;");
        break;
    default: /* no conversion required */
        break;
    }
}

/*
 * Push arguments from right to left recursively.
 * Maintain the stack aligned.
 */
unsigned function_argument(ExecNode *arg, DeclList *param)
{
    Token ty_cat;
    Declaration ty;
    unsigned arg_area_size, real_arg_size, aligned_arg_size;

    if (arg == NULL)
        return 0;

    if (param->decl->idl==NULL || param->decl->idl->op!=TOK_ELLIPSIS) {
        /*
         * This argument match a declared (non-optional) parameter.
         */
        arg_area_size = function_argument(arg->sibling, param->next);
        ty.decl_specs = param->decl->decl_specs;
        /* just get the type part of the declarator, skip any identifier */
        if (param->decl->idl!=NULL && param->decl->idl->op==TOK_ID)
            ty.idl = param->decl->idl->child;
        else
            ty.idl = param->decl->idl;
        expr_convert(arg, &ty);
        real_arg_size = compute_sizeof(&ty);
    } else {
        /*
         * This and the follow arguments match the `...'.
         */
        arg_area_size = function_argument(arg->sibling, param);
        ty = arg->type;
        expression(arg, FALSE);
        real_arg_size = compute_sizeof(&arg->type);
    }
    aligned_arg_size = round_up(real_arg_size, VM_STACK_ALIGN);
    arg_area_size += aligned_arg_size;

    /*
     * Copy struct/unions by value.
     */
    if ((ty_cat=get_type_category(&ty))==TOK_STRUCT || ty_cat==TOK_UNION) {
        emit("ldn %u;", real_arg_size);
        emit("addsp %u;", aligned_arg_size-VM_STACK_ALIGN);
    }

    return arg_area_size;
}

void expression(ExecNode *e, int is_addr)
{
    switch (e->kind.exp) {
    case OpExp:
        switch (e->attr.op) {
        case TOK_COMMA:
            expression(e->child[0], FALSE);
            emit("pop;");
            expression(e->child[1], FALSE);
            break;
        case TOK_ASSIGN:
            expr_convert(e->child[1], &e->type);
            expression(e->child[0], TRUE);
            store(&e->type);
            break;
        case TOK_MUL_ASSIGN:
        case TOK_DIV_ASSIGN:
        case TOK_MOD_ASSIGN:
        case TOK_PLUS_ASSIGN:
        case TOK_MINUS_ASSIGN:
        case TOK_LSHIFT_ASSIGN:
        case TOK_RSHIFT_ASSIGN:
        case TOK_BW_AND_ASSIGN:
        case TOK_BW_XOR_ASSIGN:
        case TOK_BW_OR_ASSIGN: {
            ExecNode new_e;

            new_e = *e;
            switch (e->attr.op) {
                case TOK_MUL_ASSIGN:    new_e.attr.op = TOK_MUL;     break;
                case TOK_DIV_ASSIGN:    new_e.attr.op = TOK_DIV;     break;
                case TOK_MOD_ASSIGN:    new_e.attr.op = TOK_MOD;     break;
                case TOK_PLUS_ASSIGN:   new_e.attr.op = TOK_PLUS;    break;
                case TOK_MINUS_ASSIGN:  new_e.attr.op = TOK_MINUS;   break;
                case TOK_LSHIFT_ASSIGN: new_e.attr.op = TOK_LSHIFT;  break;
                case TOK_RSHIFT_ASSIGN: new_e.attr.op = TOK_RSHIFT;  break;
                case TOK_BW_AND_ASSIGN: new_e.attr.op = TOK_BW_AND;  break;
                case TOK_BW_XOR_ASSIGN: new_e.attr.op = TOK_BW_XOR;  break;
                case TOK_BW_OR_ASSIGN:  new_e.attr.op = TOK_BW_OR;   break;
            }
            new_e.type.decl_specs = (TypeExp *)e->child[2];
            new_e.type.idl = (TypeExp *)e->child[3];
            expr_convert(&new_e, &e->type);
            expression(e->child[0], TRUE);
            store(&e->type);
            break;
        }

        case TOK_CONDITIONAL: {
            /*
             * e1 ? e2 : e3
             */

            unsigned L1, L2;

            L1 = new_label();
            L2 = new_label();
            /* e1 */
            expression(e->child[0], FALSE);
            emit_jmpf(L1);
            /* e2 */
            expression(e->child[1], FALSE);
            emit_jmp(L2);
            /* e3 */
            emit_lab(L1);
            expression(e->child[2], FALSE);
            emit_lab(L2);
            break;
        }

        case TOK_OR: {
            unsigned L1, L2;

            L1 = new_label();
            L2 = new_label();

            expression(e->child[0], FALSE);
            emit_jmpt(L1);
            expression(e->child[1], FALSE);
            emit_jmpt(L1);
            emit("ldi 0;");
            emit_jmp(L2);
            emit_lab(L1);
            emit("ldi 1;");
            emit_lab(L2);
            break;
        }
        case TOK_AND: {
            unsigned L1, L2;

            L1 = new_label();
            L2 = new_label();

            expression(e->child[0], FALSE);
            emit_jmpf(L1);
            expression(e->child[1], FALSE);
            emit_jmpf(L1);
            emit("ldi 1;");
            emit_jmp(L2);
            emit_lab(L1);
            emit("ldi 0;");
            emit_lab(L2);
            break;
        }

#define BIN_OPS() expression(e->child[0], FALSE), expression(e->child[1], FALSE)
        case TOK_BW_OR:
            BIN_OPS();
            emit("or;");
            break;
        case TOK_BW_XOR:
            BIN_OPS();
            emit("xor;");
            break;
        case TOK_BW_AND:
            BIN_OPS();
            emit("and;");
            break;

        case TOK_EQ:
            BIN_OPS();
            emit("eq;");
            break;
        case TOK_NEQ:
            BIN_OPS();
            emit("neq;");
            break;
        case TOK_LT:
        case TOK_GT:
        case TOK_LET:
        case TOK_GET: {
            Token cat1, cat2;

            BIN_OPS();
            cat1 = get_type_category(&e->child[0]->type);
            cat2 = get_type_category(&e->child[1]->type);
            if (is_integer(cat1) && is_integer(cat2)) {
                if (is_unsigned_int(get_promoted_type(cat1))
                || is_unsigned_int(get_promoted_type(cat2)))
                    goto relational_unsigned;
                else
                    goto relational_signed;
            } else { /* at least one of the operands has pointer type */
                goto relational_unsigned;
            }
relational_signed:
            switch (e->attr.op) {
            case TOK_LT: emit("slt;"); break;
            case TOK_GT: emit("sgt;"); break;
            case TOK_LET: emit("slet;"); break;
            case TOK_GET: emit("sget;"); break;
            }
            break;
relational_unsigned:
            switch (e->attr.op) {
            case TOK_LT: emit("ult;"); break;
            case TOK_GT: emit("ugt;"); break;
            case TOK_LET: emit("ulet;"); break;
            case TOK_GET: emit("uget;"); break;
            }
            break;
        }

        case TOK_LSHIFT:
            BIN_OPS();
            emit("sll;");
            break;
        case TOK_RSHIFT:
            BIN_OPS();
            if (is_unsigned_int(get_type_category(&e->type)))
                emit("srl;");
            else
                emit("sra;");
            break;

        case TOK_PLUS:
            BIN_OPS();
            emit("add;");
            break;
        case TOK_MINUS:
            BIN_OPS();
            emit("sub;");
            break;

        case TOK_MUL:
            BIN_OPS();
            emit("mul;");
            break;
        case TOK_DIV:
            BIN_OPS();
            if (is_unsigned_int(get_type_category(&e->type)))
                emit("udiv;");
            else
                emit("sdiv;");
            break;
        case TOK_MOD:
            BIN_OPS();
            if (is_unsigned_int(get_type_category(&e->type)))
                emit("umod;");
            else
                emit("smod;");
            break;

        case TOK_CAST:
            expr_convert(e->child[0], (Declaration *)e->child[1]);
            break;

        case TOK_PRE_INC:
        case TOK_PRE_DEC:
            expression(e->child[0], TRUE);
            emit("dup;");
            emit("dup;");
            load(e);
            if (is_integer(get_type_category(&e->type))) {
                emit("ldi 1;");
            } else { /* pointer */
                Declaration pointed_to_ty;

                pointed_to_ty.decl_specs = e->type.decl_specs;
                pointed_to_ty.idl = e->type.idl->child;
                emit("ldi %u;", compute_sizeof(&pointed_to_ty));
            }
            (e->attr.op == TOK_PRE_INC) ? emit("add;") : emit("sub;");
            emit("swap;");
            store(&e->type);
            emit("pop;");
            /* reload incremented/decremented value */
            load(e);
            break;
        case TOK_POS_INC:
        case TOK_POS_DEC:
            expression(e->child[0], TRUE);
            emit("dup;");
            load(e);
            emit("swap;");
            emit("dup;");
            load(e);
            if (is_integer(get_type_category(&e->type))) {
                emit("ldi 1;");
            } else { /* pointer */
                Declaration pointed_to_ty;

                pointed_to_ty.decl_specs = e->type.decl_specs;
                pointed_to_ty.idl = e->type.idl->child;
                emit("ldi %u;", compute_sizeof(&pointed_to_ty));
            }
            (e->attr.op == TOK_POS_INC) ? emit("add;") : emit("sub;");
            emit("swap;");
            store(&e->type);
            emit("pop;");
            break;
        case TOK_ADDRESS_OF:
            expression(e->child[0], TRUE);
            break;
        case TOK_INDIRECTION:
            expression(e->child[0], FALSE);
            if (!is_addr)
                load(e);
            break;

        case TOK_UNARY_PLUS:
            expression(e->child[0], FALSE);
            break;
        case TOK_UNARY_MINUS:
            expression(e->child[0], FALSE);
            emit("neg;");
            break;
        case TOK_COMPLEMENT:
            expression(e->child[0], FALSE);
            emit("cmpl;");
            break;
        case TOK_NEGATION:
            expression(e->child[0], FALSE);
            emit("not;");
            break;

        case TOK_SUBSCRIPT:
            if (is_pointer(get_type_category(&e->child[0]->type))) {
                /* a[i] */
                expression(e->child[0], FALSE);
                expression(e->child[1], FALSE);
            } else {
                /* i[a] */
                expression(e->child[1], FALSE);
                expression(e->child[0], FALSE);
            }
            emit("ldi %u;", compute_sizeof(&e->type));
            emit("mul;");
            emit("add;");
            if (!is_addr)
                load(e);
            break;
        case TOK_FUNCTION: {
            unsigned arg_siz;

            arg_siz = function_argument(e->child[1], e->locals);
            expression(e->child[0], FALSE);
            emit("call %u;", arg_siz);
            break;
        }
        case TOK_DOT:
        case TOK_ARROW: {
            StructMember *m;

            expression(e->child[0], FALSE);
            m = get_member_descriptor(get_type_spec(e->child[0]->type.decl_specs), e->child[1]->attr.str);
            emit("ldi %u;", m->offset);
            emit("add;");
            if (!is_addr)
                load(e);
            break;
        }
        } /* switch (e->attr.op) */
        break;
    case IConstExp:
        emit("ldi %lu;", e->attr.uval);
        break;
    case StrLitExp:
        emit("ldi @S%u;", new_string_literal(e->attr.str));
        break;
    case IdExp:
        load_addr(e);
        if (!is_addr)
            load(e);
        break;
    } /* switch (e->kind.exp) */
}

void store(Declaration *dest_ty)
{
    switch (get_type_category(dest_ty)) {
    case TOK_CHAR:
    case TOK_SIGNED_CHAR:
    case TOK_UNSIGNED_CHAR:
        emit("stb;");
        break;
    case TOK_SHORT:
    case TOK_UNSIGNED_SHORT:
        emit("stw;");
        break;
    case TOK_INT:
    case TOK_LONG:
    case TOK_ENUM:
    case TOK_UNSIGNED:
    case TOK_UNSIGNED_LONG:
    case TOK_STAR:
    case TOK_SUBSCRIPT: /* ? */
    case TOK_FUNCTION:  /* ? */
        emit("stdw;");
        break;
    case TOK_STRUCT:
    case TOK_UNION:
        emit("swap;");
        emit("memcpy %u;", compute_sizeof(dest_ty));
        break;
    }
}

void load(ExecNode *e)
{
    Token cat;

    cat = get_type_category(&e->type);
    switch (cat) {
    case TOK_STRUCT:
    case TOK_UNION:
        break;
    case TOK_FUNCTION:
    case TOK_SUBSCRIPT:
        /* load the start address (already done) */
        break;
    case TOK_STAR:
    case TOK_LONG:
    case TOK_UNSIGNED_LONG:
    case TOK_INT:
    case TOK_UNSIGNED:
        emit("lddw;");
        break;
    case TOK_SHORT:
        emit("ldw;");
        break;
    case TOK_UNSIGNED_SHORT:
        emit("lduw;");
        break;
    case TOK_CHAR:
    case TOK_SIGNED_CHAR:
        emit("ldb;");
        break;
    case TOK_UNSIGNED_CHAR:
        emit("ldub;");
        break;
    }
}

void load_addr(ExecNode *e)
{
    if (e->extra[ATTR_DURATION] == DURATION_STATIC) {
        if (e->extra[ATTR_LINKAGE] == LINKAGE_NONE) /* static local */
            emit("ldi @%s_%s;", curr_func, e->attr.str); /* use the mangled name */
        else /* external */
            emit("ldi %s;", e->attr.str);
    } else { /* parameter or local */
        int offset;

        offset = lookup_location(e->attr.str)->offset;
        emit("ldbp %u; #(%d)", offset, offset);
    }
}
