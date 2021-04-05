#include <iostream>
using namespace std;

const string utable[] = {" the"," in"," do"," an"," a"," is"," of"," and"," it"," by"
                        ," is"," has"," had"," have"," are"," was"
                        ," out"," you"," to"," there"
                        ,"ing ","ed ","es "};
const int nr_utable = sizeof(utable)/sizeof(*utable);

bool match(string const& a, size_t pos, string const& b) {
	if (pos + b.size() > a.size()) return false;
	for (size_t i = 0 ; i < b.size() ; ++i) {
		if (a[pos+i] != b[i]) return false;
	}
	return true;
}

string what,str;
int main() {
	getline(cin,what);
	getline(cin,str);
	if (what[0] != 'd') {
		// compres
		for (size_t i = 0 ; i < str.size() ; ++i) {
			// find in utable
			int j = 0;
			for ( ; j < nr_utable ; ++j) {
				if (match(str,i,utable[j])) break;
			}
			// else: use camel case
			char c = str[i];
			if (j < nr_utable) {
				if (j < 10) {
					cout << (char)(j + '0');
				} else {
					cout << "." << (char)(j - 10 + 'a');
				}
				i += utable[j].size() - 1;
			} else if (c == ' ' && i + 1 < str.size() && islower(str[i+1])) {
				c = str[++i];
				cout << (char)toupper(c);
			} else if (isupper(c) || isdigit(c) || c == '.') {
				cout << "." << c;
			} else {
				cout << c;
			}
		}
	} else {
		// decompres
		for (size_t i = 0 ; i < str.size() ; ++i) {
			char c = str[i];
			if (isupper(c)) {
				cout << ' ' << (char)tolower(c);
			} else if (isdigit(c)) {
				cout << utable[c - '0'];
			} else if (c == '.') {
				c = str[++i];
				if (c >= 'a' && c <= 'z') {
					cout << utable[c - 'a' + 10];
				} else {
					cout << c;
				}
			} else {
				cout << c;
			}
		}
	}
	cout << endl;
	return 0;
}
