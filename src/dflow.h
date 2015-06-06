#ifndef DFLOW_H_
#define DFLOW_H_

void dflow_dominance(void);

void dflow_LiveOut(void);
void compute_liveness_and_next_use(void);

#include "bset.h"

void dflow_PointOut(void);
/*
 * Return a list of targets we know the pointer p may point to at program point i.
 * Return NULL if we know nothing about where p may point to at program point i.
 */
BSet *get_pointer_targets(int i, int p);
void free_PointOut(void);

#endif
