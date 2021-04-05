
// ---------------------------------------------------------------------------
//
// Opgave:     Downhill TSP
// Uitwerking: Twan
//
// 0:18 - 1:06 (inclusief bugfixen van stomme stl iterator)
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

int solve() {
	sort(points, points + num_points);
	typedef std::map<int,int> IntMap;
	IntMap dists;
	int best = 0;
	for (int i = 0 ; i < num_points ; ++i) {
		// find number of points before here
		// lower_bound = Returns an iterator to the first element in a map that has a key value that is equal to or greater than that of a specified key.
		//             = Finds the first element whose key is not less than k.
		// so use -y, then we get the last key element with y less than or equal our y
		IntMap::iterator before = dists.lower_bound( -points[i].y );
		int dist = before == dists.end() ? 1 : before->second + 1;
		if (verbose) {
			fprintf(stderr, "reached (%d,%d) with %d  from y=%d\n", points[i].x, points[i].y, dist, before == dists.end() ? -1 : before->first);
		}
		// now remove everyhting with distance <= dist and a > y value
		// so they have a <= -y value
		IntMap::iterator after = before;
		bool should_have_exited_the_loop_already = false;
		if (after != dists.end()) {
			if (after == dists.begin()) goto removed; // don't move before the start
			--after;
		}
		while (after != dists.end()) {
			assert(-after->first >= points[i].y);
			if (after->second > dist) {
				if (verbose) {
					fprintf(stderr, " keeping y=%d with %d\n", -after->first, after->second);
				}
				if (!check) break; // this should be the end of it
				should_have_exited_the_loop_already = true;
				if (after == dists.begin()) goto removed; // don't move before the start
				--after;
			} else {
				assert(after->second >= dist-1); // it shouldn't be that bad
				assert(!should_have_exited_the_loop_already);
				if (verbose) {
					fprintf(stderr, " removing y=%d with %d\n", -after->first, after->second);
				}
				// remove
				IntMap::iterator removeme = after;
				if (after == dists.begin()) { dists.erase(removeme); goto removed;} // don't move before the start
				--after;
				dists.erase(removeme);
			}
		}
		removed:;
		// now insert the new value
		dists.insert(make_pair(-points[i].y, dist));
		best = std::max(best, dist);
		if (verbose >= 2) {
			fprintf(stderr, " now [");
			for (IntMap::iterator it = dists.begin() ; it != dists.end() ; ++it) {
				if (it != dists.begin()) fprintf(stderr, ", ");
				fprintf(stderr, "%d=%d",-it->first,it->second);
			}
			fprintf(stderr, "]\n");
		}
	}
	return best;
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
