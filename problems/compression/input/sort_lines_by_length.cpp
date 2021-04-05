#include <vector>
#include <string>
#include <algorithm>
#include <iostream>
using namespace std;

vector<string> lines;

bool compare_length(string const& a,string const& b) {
	return a.size() > b.size();
}

int main() {
	while (cin) {
		string l;
		getline(cin,l);
		if (l.empty()) break;
		lines.push_back(l);
	}
	cerr << "Found " << lines.size() << " lines" << endl;
	sort(lines.begin(), lines.end(), compare_length);
	for (int i = 0 ; i < lines.size() ; ++i) {
		cout << lines[i] << endl;
	}
	return 0;
}
