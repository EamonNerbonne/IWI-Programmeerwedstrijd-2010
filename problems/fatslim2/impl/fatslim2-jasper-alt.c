#include <stdio.h>
#include <stdlib.h>

#define START_TIME (4*60*60)
#define TRAIN_TIME (8*60*60)
#define MAX_PEOPLE (1000000)
#define MAX_GATES (10)
#define MAX_MACHINES (10)

int np, ntp;
int people[MAX_PEOPLE];
int ticketPeople[MAX_PEOPLE][2];

int minElement(int* a, int n) {
    int mini = 0, i;
    for(i=1; i<n; i++) {
        if (a[i]<a[mini]) {
            mini = i;
        }
    }
    return mini;
}

int mymax(int a, int b) {
    return a>=b?a:b;
}

int compareInts(const void* a_, const void* b_) {
    int *a = (int*)a_;
    int *b = (int*)b_;
    return *a - *b;
}

void run() {
    int ng, nt;
    int n;
    int gates[MAX_GATES];
    int ticketmachines[MAX_MACHINES];
    int g, t, i;
    int miss;

    // Read input
    scanf("%d %d",&ng,&nt);
    scanf("%d",&n);
    np = 0; ntp = 0;
    for(i=0; i<n; i++) {
        int t, d;
        scanf("%d %d",&t,&d);
        if (d==0) {
            people[np] = t;
            np++;
        } else {
            ticketPeople[ntp][0] = t;
            ticketPeople[ntp][1] = d;
            ntp++;
        }
    }

    // Initialize
    for(g=0; g<ng; g++) {
        gates[g] = 0;
    }
    for(t=0; t<nt; t++) {
        ticketmachines[t] = 0;
    }

    // Simulate ticket booths
    for(i=0; i<ntp; i++) {
        int firstFreeMachine = minElement(ticketmachines,nt);
        int readyTime = mymax(ticketmachines[firstFreeMachine],ticketPeople[i][0]) + ticketPeople[i][1];
        people[np++] = readyTime;
        ticketmachines[firstFreeMachine] = readyTime;
    }
    qsort(people,np,sizeof(people[0]),compareInts);

    // Simulate gates
    miss = 0;
    for(i=0; i<np; i++) {
        int firstFreeGate = minElement(gates,ng);
        int readyTime = mymax(gates[firstFreeGate],people[i]) + 1;
        if (START_TIME+readyTime>TRAIN_TIME) miss++;
        gates[firstFreeGate] = readyTime;
    }

    // Show result
    printf("%d\n",miss);
}

int main() {
    int runs;
    scanf("%d",&runs);
    while(runs-->0) run();
    return 0;
}
