#include <algorithm>
#include <functional>
#include <queue>
#include <stdio.h>
#include <utility>
#include <vector>

const int START_TIME = 4*60*60;
const int TRAIN_TIME = 8*60*60;

void run() {
    // Read input
    int ng, nt;
    scanf("%d %d",&ng,&nt);
    int n;
    scanf("%d",&n);
    std::vector<int> people;
    std::vector< std::pair<int,int> > ticketPeople;
    for(int i=0; i<n; i++) {
        int t, d;
        scanf("%d %d",&t,&d);
        if (d==0) {
            people.push_back(t);
        } else {
            ticketPeople.push_back(std::make_pair(t,d));
        }
    }

    // Initialize
    std::priority_queue< int,std::vector<int>,std::greater<int> > gates;
    std::priority_queue< int,std::vector<int>,std::greater<int> > ticketmachines;
    for(int g=0; g<ng; g++) {
        gates.push(0);
    }
    for(int t=0; t<nt; t++) {
        ticketmachines.push(0);
    }

    // Simulate ticket booths
    for(int i=0; i<ticketPeople.size(); i++) {
        int firstFreeMachine = ticketmachines.top(); ticketmachines.pop();
        int readyTime = std::max(firstFreeMachine,ticketPeople[i].first) + ticketPeople[i].second;
        people.push_back(readyTime);
        ticketmachines.push(readyTime);
    }

    // Simulate gates
    int miss = 0;
    for(int i=0; i<people.size(); i++) {
        int firstFreeGate = gates.top(); gates.pop();
        int readyTime = std::max(firstFreeGate,people[i]) + 1;
        if (START_TIME+readyTime>TRAIN_TIME) miss++;
        gates.push(readyTime);
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
