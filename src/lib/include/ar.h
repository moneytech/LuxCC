#ifndef _AR_H
#define _AR_H

#define ARMAG   "!<arch>\n" /* magic string */
#define SARMAG  8           /* length of magic string */
#define ARFMAG  "`\n"       /* header trailer string */

struct ar_hdr {             /* file member header */
    char    ar_name[16];    /* '/' terminated file member name */
    char    ar_date[12];    /* file member date */
    char    ar_uid[6];      /* file member user identification */
    char    ar_gid[6];      /* file member group identification */
    char    ar_mode[8];     /* file member mode (octal) */
    char    ar_size[10];    /* file member size */
    char    ar_fmag[2];     /* header trailer string */
};

#endif /* ar.h */
