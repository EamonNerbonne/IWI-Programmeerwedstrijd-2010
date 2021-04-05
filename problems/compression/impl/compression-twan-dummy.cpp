// Only makes the input longer :)

#include <iostream>
using namespace std;

string what,str;
int main() {
	getline(cin,what);
	getline(cin,str);
	if (what[0] != 'd') {
		// compres
		cout << "compressed." << str << endl;
	} else {
		// decompres
		str = str.substr(11);
		cout << str << endl;
	}
	return 0;
}
