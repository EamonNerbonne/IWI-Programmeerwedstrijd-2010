
// ---------------------------------------------------------------------------
//
// Opgave:     Downhill TSP
// Uitwerking: Twan
//
// O(n^2) uitwerking, maar hiervan ben ik zeker dat het goed is
// 15:51-
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

const int MAX_POINTS = 1000000 - 1;
const int MAX_XY     = 1000000000 - 1;

// ---------------------------------------------------------------------------
// Input
// ---------------------------------------------------------------------------

struct Point {
	int x,y;
};
bool operator < (Point const& a, Point const& b) {
	return a.x < b.x || (a.x == b.x && a.y < b.y);
}

int num_points;
Point points[MAX_POINTS];

void read_input() {
	scanf("%d",&num_points);
	if (num_points >= MAX_POINTS) {
		fprintf(stderr,"Too many points: %d\n",num_points);
		exit(1);
	}
	for (int i = 0 ; i < num_points ; ++i) {
		scanf("%d %d", &points[i].x, &points[i].y);
		if (points[i].x < 0 || points[i].y < 0 || points[i].x > MAX_XY || points[i].y > MAX_XY) {
			// non-negative integer
			fprintf(stderr,"Point too large");
			exit(1);
		}
	}
}

// ---------------------------------------------------------------------------
// Solution
// ---------------------------------------------------------------------------

int best[MAX_POINTS];

int solve() {
	if (num_points == 0) return 0; // edge case
	sort(points, points + num_points);
	for (int i = 0 ; i < num_points ; ++i) {
		best[i] = 1;
		for (int j = 0 ; j < i ; ++j) {
			if (points[j].x <= points[i].x && points[j].y <= points[i].y) {
				best[i] = max(best[i], best[j] + 1);
			}
		}
		if (verbose) {
			fprintf(stderr, "reached (%d,%d) with %d\n", points[i].x, points[i].y, best[i]);
		}
	}
	return *max_element(best, best + num_points);
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
