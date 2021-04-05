// 17:01 - 17:37
#include <iostream>
using namespace std;

char to64(int x) {
	if (x < 26) return 'a'+x; else x -= 26;
	if (x < 26) return 'A'+x; else x -= 26;
	if (x < 10) return '0'+x; else x -= 10;
	return x == 0 ? ' ' : '.';
}
int from64(char x) {
	if (x >= 'a' && x <= 'z') return x-'a';
	if (x >= 'A' && x <= 'Z') return x-'A'+26;
	if (x >= '0' && x <= '9') return x-'0'+26+26;
	if (x == ' ') return 0+26+26+10;
	if (x == '.') return 1+26+26+10;
	throw "FAAL";
}

/*struct Base64Out {
	unsigned val;
	unsigned sig;
	void add(char c) {
		if (sig > 64) {
		}
	}
};*/
struct Base64Out {
	int val;
	int bits;
	Base64Out() : val(0), bits(0) {}
	void add(int x, int dbits = 5) {
		val <<= dbits;
		val |= x;
		bits += dbits;
		while (bits >= 6) {
			bits -= 6;
			cout << to64((val >> bits) & 0x3f);
			//cerr << "write " << ((val >> bits) & 0x3f) << "\n";
		}
	}
};

struct Base64In {
	char const* buf;
	int val, bits;
	Base64In(char const* buf) : buf(buf), val(0), bits(0) {}
	int get(int dbits = 5) {
		while (bits < dbits) {
			bits += 6;
			val <<= 6;
			if (!*buf) return 31;
			if (!*buf) throw "no more input";
			//cerr << "read " << from64(*buf) << " then have " << bits << "\n";
			val |= from64(*(buf++));
		}
		bits -= dbits;
		int out = val >> bits & ((1 << dbits) - 1);
		//cerr << "out " << out << endl;
		return out;
	}
};

string what,str;
int main() {
	getline(cin,what);
	getline(cin,str);
	if (what[0] != 'd') {
		// compres
		Base64Out out;
		for (size_t i = 0 ; i < str.size() ; ++i) {
			char c = str[i];
			if (c == ' ') out.add(26);
			else          out.add(c-'a');
		}
		out.add(31);
	} else {
		// decompres
		Base64In in(str.c_str());
		while (true) {
			int i = in.get();
			if (i == 31) break;
			if (i == 26) cout << ' ';
			else         cout << (char)(i+'a');
			// debug
			//static int zzz=0;if (zzz++>10) break;
		}
	}
	cout << endl;
	return 0;
}
