#include <stdio.h>
#include <stdlib.h>

#define MAXN (1000000)

typedef struct { int x, y; } Position;

int comparePositionsXY(const void* a_, const void* b_) {
    Position *a = (Position*)a_;
    Position *b = (Position*)b_;
    return a->x + a->y - b->x - b->y;
}

Position pos[MAXN];
int houses[MAXN];

void run() {
    // Input
    int n, i, j, best;
    scanf("%d",&n);
    for(i=0; i<n; i++) {
        scanf("%d %d",&(pos[i].x),&(pos[i].y));
        houses[i] = 1;
    }

    // Solve
    qsort(pos, n, sizeof(Position), comparePositionsXY);
    for(i=0; i<n; i++) {
        for(j=0; j<i; j++) {
            if (pos[j].x<=pos[i].x && pos[j].y<=pos[i].y && houses[i]<houses[j]+1) {
                houses[i] = houses[j]+1;
            }
        }
    }

    // Output
    best = 0;
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
