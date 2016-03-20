#ifndef _ASSERT_H
#define _ASSERT_H

void __assert(const char *, const char *, int);

#ifndef NDEBUG
#define assert(expr) ((expr) ? (void)0 : __assert(__FILE__, __func__, __LINE__))
#else
#define assert(expr) ((void)0)
#endif

#endif
