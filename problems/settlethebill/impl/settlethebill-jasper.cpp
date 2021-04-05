#include <stdio.h>

const int MAXN = 99;
int values[MAXN];

int zero_subsets(int s, int* v, int n) {
    if (n==0) {
        return s==0?1:0;
    } else {
        return zero_subsets(s+v[0], v+1, n-1) + zero_subsets(s, v+1, n-1);
    }
}

void run() {
    // Input
    int n, m;
    scanf("%d %d",&n,&m);
    for(int i=0; i<n; i++) {
        values[i] = 0;
    }
    for(int i=0; i<m; i++) {
        int p, q, r;
        scanf("%d %d %d",&p,&q,&r);
        values[p] -= r;
        values[q] += r;
        if (p>=n || q>=n || p<0 || q<0) {
            printf("invalid person\n");
            return;
        }
    }

    // Solve
    int zs = zero_subsets(0,values,n);

    // Output
    if (zs<2) {
        int sum = 0;
        for(int i=0; i<n; i++) {
            sum += values[i];
        }
        printf("too few zero subsets (sum=%d)\n",sum);
    } else if (zs==2) {
        printf("tight\n");
    } else {
        printf("loose\n");
    }
}

int main() {
    int runs;
    scanf("%d",&runs);
    while(runs-->0) run();
    return 0;
}
