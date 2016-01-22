/*
 * Program that solves the Josephus problem.
 * From 'Algorithms in C' by Robert Sedgewick.
 */
#include <stdio.h>
#include <stdlib.h>

typedef struct node* link;

struct node {
    int item;
    link next;
};

int main(void)
{
    int i, N = 9, M = 5;
    link t = malloc(sizeof *t), x = t;

    t->item = 1;
    t->next = t;
    for (i = 2; i <= N; i++) {
        x = (x->next = malloc(sizeof *x));
        x->item = i;
        x->next = t;
    }
    while (x != x->next) {
        for (i = 1; i < M; i++)
            x = x->next;
        x->next = x->next->next;
        N--;
    }
    printf("%d\n", x->item);
    return 0;
}
