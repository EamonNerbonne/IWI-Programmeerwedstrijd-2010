#include <stdio.h>
#include <stdlib.h>

#define MAXN (1000000)

typedef struct { int x, y, h; } Position;

int comparePositionsXY(const void* a_, const void* b_) {
    Position *a = (Position*)a_;
    Position *b = (Position*)b_;
    return a->x!=b->x ? a->x - b->x : a->y - b->y;
}

Position pos[MAXN];

void run() {
    // Input
    int n, o, i, j, m=0, best=0;
    scanf("%d",&n);
    for(i=0; i<n; i++) {
        scanf("%d %d",&(pos[i].x),&(pos[i].y));
        pos[i].h = 1;
    }

    // Solve
    qsort(pos, n, sizeof(Position), comparePositionsXY);
    for(i=0; i<n; i++) {
        for(j=0; j<m; j++) {
            if (pos[j].y<=pos[i].y && pos[i].h<pos[j].h+1) {
                pos[i].h = pos[j].h+1;
            }
        }
        o = 0;
        for(j=0; j<m; j++) {
            if (pos[j].y>pos[i].y && pos[i].h>=pos[j].h) {
                o++;
            }
            if (j+o<m) {
                pos[j] = pos[j+o];
            }
        }
        m = m+1-o;
        pos[m-1] = pos[i];
    }

    // Output
    for(i=0; i<m; i++) {
        if (pos[i].h>best) best = pos[i].h;
    }
    printf("%d\n", best);
}

int main() {
    int runs;
    scanf("%d",&runs);
    while(runs-->0) run();
    return 0;
}
