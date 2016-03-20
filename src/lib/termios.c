#include <errno.h>
#include <termios.h>
#include <sys/ioctl.h>

int tcflush(int fd, int queue_selector)
{
    return ioctl(fd, TCFLSH, (void *)queue_selector);
}
