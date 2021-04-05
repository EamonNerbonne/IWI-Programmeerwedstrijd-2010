#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_RODS (1000000)

double rod_angles[MAX_RODS];

int compare_doubles(const void* a_, const void* b_) {
    double* a = (double*)a_;
    double* b = (double*)b_;
    return *a<*b ? -1 : *a>*b ? 1 : 0;
}

void run() {
    // Input
    int n, i, j, x, y;
    double angle, best_angle;
    scanf("%d",&n);
    for(i=0; i<n; i++) {
        scanf("%d %d",&x,&y);
        rod_angles[i] = atan2(y,x);
    }

    // Solve
    best_angle = 0;
    qsort(rod_angles, n, sizeof(double), compare_doubles);
    for(i=0; i<n; i++) {
        j = (i+1)%n;
        angle = rod_angles[j]-rod_angles[i];
        if (angle<0) angle += 2*M_PI;
        if (angle>best_angle) best_angle = angle;
    }

    // Output
    printf("%.7f\n", (180.0/M_PI)*best_angle);
}

int main() {
    int runs;
    scanf("%d",&runs);
    while(runs-->0) run();
    return 0;
}
