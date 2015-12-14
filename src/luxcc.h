#ifndef LUXCC_H_
#define LUXCC_H_

extern unsigned warning_count;
extern unsigned error_count;
extern int disable_warnings;
extern int colored_diagnostics;
extern int targeting_arch64;
extern int include_liblux;
extern char *cg_outpath;
extern char *cfg_outpath;
extern char *cfg_function_to_print;
extern char *ic_outpath;
extern char *ic_function_to_print;
extern unsigned stat_number_of_pre_tokens;
extern unsigned stat_number_of_c_tokens;
extern unsigned stat_number_of_ast_nodes;

#endif
