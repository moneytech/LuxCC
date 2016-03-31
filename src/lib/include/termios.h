#ifndef _TERMIOS_H
#define _TERMIOS_H

typedef unsigned char cc_t;
typedef unsigned int speed_t;
typedef unsigned int tcflag_t;

struct termios {
    tcflag_t c_iflag;   /* input mode flags */
    tcflag_t c_oflag;   /* output mode flags */
    tcflag_t c_cflag;   /* control mode flags */
    tcflag_t c_lflag;   /* local mode flags */
    cc_t c_line;        /* line discipline */
    cc_t c_cc[32];      /* control characters */
    speed_t c_ispeed;   /* input speed */
    speed_t c_ospeed;   /* output speed */
};

#define TCIFLUSH    0
#define TCOFLUSH    1
#define TCIOFLUSH   2

int tcflush(int fd, int queue_selector);

#endif
