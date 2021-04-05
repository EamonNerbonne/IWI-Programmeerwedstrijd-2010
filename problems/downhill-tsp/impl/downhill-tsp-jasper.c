#include <stdio.h>
#include <stdlib.h>

#define MAXN (1000000)

typedef struct { int x, y; } Position;

int comparePositionsXY(const void* a_, const void* b_) {
    Position *a = (Position*)a_;
    Position *b = (Position*)b_;
    return a->x!=b->x ? a->x - b->x : a->y - b->y;
}

Position pos[MAXN];
int skip[MAXN];
int houses[MAXN];

void run() {
    // Input
    int n, i, j, prevj, firstj=0, best=0;
    scanf("%d",&n);
    for(i=0; i<n; i++) {
        scanf("%d %d",&(pos[i].x),&(pos[i].y));
        houses[i] = 1;
        skip[i] = 1;
    }

    // Solve
    qsort(pos, n, sizeof(Position), comparePositionsXY);
    for(i=0; i<n; i++) {
        for(j=firstj; j<i; j+=skip[j]) {
            if (pos[j].y<=pos[i].y && houses[i]<houses[j]+1) {
                houses[i] = houses[j]+1;
            }
        }
        prevj=-1;
        for(j=firstj; j<i; j+=skip[j]) {
            if (pos[j].y>pos[i].y && houses[i]>=houses[j]) {
                if (prevj<0) {
                    firstj += skip[j];
                } else {
                    skip[prevj] += skip[j];
                }
            }
            prevj = j;
        }
    }

    // Output
    for(i=0; i<n; i++) {
        if (houses[i]>best) best = houses[i];
    }
    printf("%d\n", best);
}

int main() {
    int runs;
    scanf("%d",&runs);
    while(runs-->0) run();
    return 0;
}
