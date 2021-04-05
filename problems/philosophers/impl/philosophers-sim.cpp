#include <stdio.h>

struct Plate {
	int drumsticks_remaining;
};
struct Philosopher {
	int drumsticks_eaten;
	bool sharing_deep_insight;
};
struct Fork {
	int owned_by;
};

Plate       plate;
Philosopher philosophers[5];
Fork        forks[5];

void init(int n) {
	plate.drumsticks_remaining = n;
	for (int i = 0 ; i < 5 ; ++i) {
		philosophers[i].drumsticks_eaten = 0;
		philosophers[i].sharing_deep_insight = false;
	}
	for (int i = 0 ; i < 5 ; ++i) {
		forks[i].owned_by = -1;
	}
}

void first() {
	for (int i = 0 ; i < 5 ; ++i) {
		forks[i].owned_by = i;
	}
	// kant is a nice guy
	forks[0].owned_by = -1;
	philosophers[0].sharing_deep_insight = true;
}

void run() {
	while (plate.drumsticks_remaining) {
		// try to take forks
		for (int i = 0 ; i < 5 ; ++i) {
			if (philosophers[i].sharing_deep_insight) {
				philosophers[i].sharing_deep_insight = false; // already ate last turn
			} else {
				if (forks[i      ].owned_by == -1) forks[i]      .owned_by = i;
				if (forks[(i+1)%5].owned_by == -1) forks[(i+1)%5].owned_by = i;
			}
		}
		// have two forks?
		for (int i = 0 ; i < 5 ; ++i) {
			if (forks[i].owned_by == i && forks[(i+1)%5].owned_by == i) {
				// eat
				plate.drumsticks_remaining--;
				philosophers[i].drumsticks_eaten++;
				philosophers[i].sharing_deep_insight = true;
				// release forks
				forks[i      ].owned_by = -1;
				forks[(i+1)%5].owned_by = -1;
			}
		}
	}
}

int main() {
	int runs,n;
	scanf("%d",&runs);
	while (runs --> 0) {
		scanf("%d",&n);
		init(n);
		first();
		run();
		printf("%d\n",philosophers[0].drumsticks_eaten);
	}
	return 0;
}
