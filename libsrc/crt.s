# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                               C Runtime Startup                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#
# Globals.
#
.bss
    .align 4
_vars:
stdin:
.global stdin
    .res 4
stdout:
.global stdout
    .res 4
stderr:
.global stderr
    .res 4
_argc:
    .res 4
_argv:
    .res 4

#
# Entry point.
#
.text
    # let the VM set globals
    ldi _vars;
    ldi _getvars;
    call 4;
    pop;

    # call main/exit
    .extern main    # the user must define it!
    ldi _argv;
    lddw;
    ldi _argc;
    lddw;
    ldi main;
    call 8;
    halt;
    #ldi exit;
    #call 4;

#
# Library routines.
#
_getvars:
    libcall 0;
    ret;
malloc:
.global malloc
    libcall 1;
    ret;
free:
.global free
    libcall 2;
    ret;
exit:
.global exit
    libcall 3;
    ret;
realloc:
.global realloc
    libcall 4;
    ret;
fputc:
.global fputc
    libcall 5;
    ret;
fgetc:
.global fgetc
    libcall 6;
    ret;
fread:
.global fread
    libcall 7;
    ret;
fwrite:
.global fwrite
    libcall 8;
    ret;
ferror:
.global ferror
    libcall 9;
    ret;
fopen:
.global fopen
    libcall 10;
    ret;
fclose:
.global fclose
    libcall 11;
    ret;
fgets:
.global fgets
    libcall 12;
    ret;
# missing
getopt_long: .global getopt_long
optarg: .global optarg
optind: .global optind
qsort: .global qsort
bsearch: .global bsearch
calloc: .global calloc
errno: .global errno
strtol: .global strtol
strtoul: .global strtoul
sscanf: .global sscanf
strdup: .global strdup
strrchr: .global strrchr
fseek: .global fseek
ftell: .global ftell
rewind: .global rewind
