
// ---------------------------------------------------------------------------
//
// Opgave:     Ticket inspection
// Uitwerking: Twan
//
// 0:03 - 
//
// ---------------------------------------------------------------------------

#include <cstdio>
#include <cstring>
#include <cctype>
#include <cmath>
#include <cassert>
#include <algorithm>
#include <vector>
#include <map>
#include <string>
using namespace std;

int verbose = 0;
int check = 0;

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const int MAX_CARS = 100 - 1;
const int MAX_STEPS = 100 - 1;

// ---------------------------------------------------------------------------
// Input
// ---------------------------------------------------------------------------

int num_cars;
int num_steps;
char grid[2][2 * MAX_CARS + 1];

void read_input() {
	scanf("%d %d ",&num_cars, &num_steps);
	if (num_cars >= MAX_CARS) {
		fprintf(stderr,"Too many cars: %d\n",num_cars);
		exit(1);
	}
	if (num_steps >= MAX_CARS) {
		fprintf(stderr,"Too many time steps: %d\n",num_steps);
		exit(1);
	}
	for (int i = 0 ; i < 2 ; ++i) {
		fgets(grid[i], 2*num_cars+1, stdin);
	}
}

// ---------------------------------------------------------------------------
// Solution
// ---------------------------------------------------------------------------

struct State {
	// TODO
};

int solve() {
	return 0; // TODO
}

// ---------------------------------------------------------------------------
// Main program
// ---------------------------------------------------------------------------

void run() {
	read_input();
	int n = solve();
	printf("%d\n",n);
}

int main(int argc, char** argv) {
	// Parse args
	for (int i = 1 ; i < argc ; ++i) {
		string arg = argv[i];
		if (arg.size() > 2 && arg.substr(0,2) == "--") arg = arg.substr(1);
		if (arg == "-v" || arg=="-verbose") {
			verbose = 1;
		} else if (arg == "-v2") {
			verbose = 2;
		} else if (arg == "-c" || arg=="-check") {
			check = 1;
		} else {
			fprintf(stderr, "Unknown argument: %s", argv[i]);
			exit(1);
		}
	}
	// Number of runs
	int runs;
	scanf("%d", &runs);
	for (int i = 0 ; i < runs ; ++i) {
		if (verbose) {
			fprintf(stderr, "\nRun %d:\n", i);
		}
		run();
	}
	return 0;
}
