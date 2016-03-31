#ifndef _IOCTL_H
#define _IOCTL_H

#if defined __i386__ || defined __x86_64__ || defined __arm__
/*
 * More values and corresponding `arg' type
 * for these platforms can be found here:
 *      http://man7.org/linux/man-pages/man2/ioctl_list.2.html
 */
#define TCGETS 0x00005401
#define TCFLSH 0x0000540B
#elif defined __mips__
#define TCFLSH 0x00005407
#define TCGETS 0x0000540D
#endif

int ioctl(int fd, unsigned long request, void *arg);

#endif
