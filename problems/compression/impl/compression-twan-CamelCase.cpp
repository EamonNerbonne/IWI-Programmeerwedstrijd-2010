#include <iostream>
using namespace std;

string what,str;
int main() {
	getline(cin,what);
	getline(cin,str);
	if (what[0] != 'd') {
		// compres
		for (size_t i = 0 ; i < str.size() ; ++i) {
			char c = str[i];
			if (c == ' ' && i + 1 < str.size() && islower(str[i+1])) {
				c = str[++i];
				cout << (char)toupper(c);
			} else if (isupper(c) || c == '.') {
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
			} else if (c == '.') {
				cout << str[++i];
			} else {
				cout << c;
			}
		}
	}
	cout << endl;
	return 0;
}
