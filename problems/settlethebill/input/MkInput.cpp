#include <stdio.h>
#include <stdlib.h>

int main(int argc, const char *argv[]) {
    int runs, seed, n, m;
    sscanf(argv[1],"%d",&runs);
    sscanf(argv[2],"%d",&seed);
    sscanf(argv[3],"%d",&n);
    sscanf(argv[4],"%d",&m);
    srand(seed);
    printf("%d\n",runs);
    for(int r=0; r<runs; r++) {
        printf("%d %d\n",n,m);
        for(int i=0; i<m; i++) {
            int f, t;
            do {
                f = rand()%n;
                t = rand()%n;
            } while(f==t);
            int a = (rand()%1000)*10000 + (rand()%10000);
            printf("%d %d %d\n",f,t,a);
        }
    }
    return 0;
}
