#include <time.h>
#include <stdio.h>
#include <sys/times.h>
#include <sys/syscall.h>

#define CLK_TCK 100 /* clock ticks per second in Linux */

clock_t clock(void)
{
    struct tms s;
    clock_t res;

    if ((res=times(&s)) != (clock_t)-1)
        /*
         We could define CLOCKS_PER_SEC to be the same as CLK_TCK (100 in Linux):
         X: clock ticks

             X
            ---     == # seconds
          CLK_TCK

         But POSIX requires CLOCKS_PER_SEC to be 1000000, so we need to account for that:

             X
            ---     *  CLOCKS_PER_SEC
          CLK_TCK
         -------------------------------- == # seconds
                CLOCKS_PER_SEC
        */
        res = (s.tms_utime+s.tms_stime/*+s.tms_cutime+s.tms_cstime*/)*(CLOCKS_PER_SEC/CLK_TCK);
    return res;
}

time_t time(time_t *timer)
{
    return syscall(SYS_time, timer);
}

char *asctime(const struct tm *timeptr)
{
    static const char wday_name[7][3] = {
        "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
    };
    static const char mon_name[12][3] = {
        "Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
    };
    static char result[26];

    sprintf(result, "%.3s %.3s%3d %.2d:%.2d:%.2d %d\n",
        wday_name[timeptr->tm_wday],
        mon_name[timeptr->tm_mon],
        timeptr->tm_mday, timeptr->tm_hour,
        timeptr->tm_min, timeptr->tm_sec,
        1900+timeptr->tm_year);
    return result;
}
