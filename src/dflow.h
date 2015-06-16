#ifndef DFLOW_H_
#define DFLOW_H_

void dflow_dominance(unsigned fn);
void dflow_LiveOut(unsigned fn);
#include "bset.h"
void dflow_PointOut(void);
void dflow_summaries(void);
/*
 * Return a list of targets we know the pointer p may point to at program point i.
 * Return NULL if we know nothing about where p may point to at program point i.
 */
BSet *get_pointer_targets(int i, int p);
void free_PointOut(void);

#endif
