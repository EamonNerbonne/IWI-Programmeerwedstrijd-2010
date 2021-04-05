#include <iostream>
#include <map>
#include <vector>
using namespace std;

char to62(int x) {
	if (x < 26) return 'a'+x; else x -= 26;
	if (x < 26) return 'A'+x; else x -= 26;
	if (x < 10) return '0'+x; else x -= 10;
	throw "Bwaak";
	//return x == 0 ? ' ' : '.';
}
int from62(char x) {
	if (x >= 'a' && x <= 'z') return x-'a';
	if (x >= 'A' && x <= 'Z') return x-'A'+26;
	if (x >= '0' && x <= '9') return x-'0'+26+26;
	//if (x == ' ') return 0+26+26+10;
	//if (x == '.') return 1+26+26+10;
	throw "FAAL";
}

/*
string encode(int i) {
	string s;
	int a = i % 36; i /= 36;
	s += a < 10 ? (char)('0'+i) : (char)('A'+i-10);
	while (i) {
		int a = i % 25; i /= 26;
		s += a < 10 ? (char)('A'+i-10);
	}
	return s;
}*/

map<string,int> word_counts;
map<string,int> words;
vector<string> dewords;

/*void write_int(int i) {
	int n = words.size();
	while (n) {
		cout << to62(i % 62); i /= 62;
		n /= 62;
	}
}*/

void write_int(int i) {
	while (true) {
		if (i < 31) {
			cout << to62(i % 31); i /= 31;
			break;
		} else {
			cout << to62(i % 31 + 31); i /= 31;
		}
	}
}


void add(string const& word) {
	int n = ++word_counts[word];
	if (n >= 1) {
		int i = (int)words.size();
		words[word] = i;
		dewords.push_back(word);
	}
}

bool hasupper(string const& word) {
	for (size_t i = 0 ; i < word.size() ; ++i) if (isupper(word[i])) return true;
	return false;
}

void compress_word(string const& word) {
	map<string,int>::const_iterator it = words.find(word);
	if (it == words.end()) {
		add(word);
		//cout << "." << word << " ";
		if (hasupper(word) || !islower(word[word.size()-1])) {
			cout << " ";
			cout << to62(word.size());
			cout << word;
		} else {
			cout << "." << word.substr(0,word.size()-1) << (char)toupper(word[word.size()-1]);
		}
	} else {
		write_int(it->second);
	}
}

bool isword(char x) {
	return x != ' ' && x != '.';
}

void init() {
	add(" the");
	add(" a");
}

string what,str;
int main() {
	getline(cin,what);
	getline(cin,str);
	init();
	if (what[0] != 'd') {
		// compres
		for (size_t i = 0 ; i < str.size() ; ) {
			// find the next word
			size_t j = i+1;
			while (j < str.size() && j<i+62 && isword(str[j])) ++j;
			// [i..j) is a word
			//if (j < str.size() && isword(str[i]) && str[j] == ' ') j++;
			// compress it
			compress_word(str.substr(i,j-i));
			// the space after the word
			i = j;
		}
	} else {
		// decompres
		for (size_t i = 0 ; i < str.size() ; ) {
			if (str[i] == ' ') {
				// add a word
				size_t n = from62(str[i+1]);
				string word = str.substr(i+2,n);
				add(word);
				cout << word;
				i = i + 2 + n;
			} else if (str[i] == '.') {
				// add a word
				size_t j = i+1; while (!isupper(str[j])) ++j;
				string word = str.substr(i+1,j-i-1);
				word += (char)tolower(str[j]);
				add(word);
				cout << word;
				i = j + 1;
			} else {
				int nr = 0;
				int w = 1;
				/*int n = words.size();
				while (n) {
					nr += from62(str[i++]) * w;
					n /= 62; w *= 62;
				}
				*/
				while (true) {
					int b = from62(str[i++]);
					nr += (b%31) * w;
					if (b < 31) break;
					w *= 31;
				}
				cout << dewords[nr];
			}
		}
	}
	cout << endl;
	return 0;
}
