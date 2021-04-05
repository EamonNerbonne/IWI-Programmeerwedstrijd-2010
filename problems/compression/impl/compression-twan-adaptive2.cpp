#include <iostream>
#include <map>
#include <vector>
#include <algorithm>
#include <utility>
#include <climits>
using namespace std;

#define SIMPLE_WORD 0
#define NUM_WORDS 0

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
int read_int(char const*& str) {
	int nr = 0, w = 1;
	while (true) {
		int b = from62(*str++);
		nr += (b%31) * w;
		if (b < 31) break;
		w *= 31;
	}
	return nr;
}

bool hasupper(string const& word) {
	for (size_t i = 0 ; i < word.size() ; ++i) if (isupper(word[i])) return true;
	return false;
}
bool islowersp(char x) { return islower(x) || x == ' '; }
bool only_lower(string const& word) {
	if (word.empty()) return false;
	for (size_t i = 0 ; i < word.size() ; ++i) if (!islowersp(word[i])) return false;
	return true;
}

void write_word(string const& word) {
	#if SIMPLE_WORD
		write_int((int)word.size());
		cout << word;
	#else
		if (only_lower(word) && islower(word[word.size()-1])) {
			cout << word.substr(0,word.size()-1) << (char)toupper(word[word.size()-1]);
		} else if (word.size() < 10) {
			cout << word.size() << word;
		} else {
			cout << ".";
			write_int((int)word.size());
			cout << word;
		}
	#endif
}
string read_word(char const*& str) {
	#if SIMPLE_WORD
		int len = read_int(str);
		char const* begin = str;
		str += len;
		return string(begin, str);
	#else
		if (islowersp(*str)) {
			string word;
			while (!isupper(*str)) {
				word += *str++;
			}
			word += (char)tolower(*str++);
			return word;
		} else if (*str == '.') {
			++str;
			int len = read_int(str);
			char const* begin = str;
			str += len;
			return string(begin, str);
		} else {
			int len = (*str++) - '0';
			char const* begin = str;
			str += len;
			return string(begin, str);
		}
	#endif
}

/*
void add(string const& word) {
	int n = ++word_counts[word];
	if (n >= 1) {
		int i = (int)words.size();
		words[word] = i;
		dewords.push_back(word);
	}
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
}*/

map<string,int> word_counts;
map<string,int> words;
vector<string> dewords;
vector<string> text;

bool compare_snd(pair<string,int> const& a, pair<string,int> const& b) {
	return a.second > b.second;
}
void build_table() {
	// sort by word_counts
	vector<pair<string,int> > wc; wc.reserve(word_counts.size());
	for (map<string,int>::const_iterator it = word_counts.begin() ; it != word_counts.end() ; ++it) {
		wc.push_back(*it);
	}
	sort(wc.begin(),wc.end(),compare_snd);
	// add words
	for (int i = 0 ; i < (int)wc.size() ; ++i) {
		words[wc[i].first] = i;
		dewords.push_back(wc[i].first);
	}
}

bool isword(char x) {
	return x != ' ' && x != '.';
}

void init() {
}

string what,str;
int main() {
	getline(cin,what);
	getline(cin,str);
	init();
	char const* cstr = str.c_str();
	if (what[0] != 'd') {
		// build text table
		for (size_t i = 0 ; i < str.size() ; ) {
			// find the next word
			size_t j = i+1;
			while (j < str.size() && j<i+62 && isword(str[j])) ++j;
			// [i..j) is a word
			// compress it
			string word = str.substr(i,j-i);
			text.push_back(word);
			word_counts[word]++;
			// the space after the word
			i = j;
		}
		build_table();
		// write table
		if (NUM_WORDS) write_int((int)dewords.size());
		for (size_t i = 0 ; i < dewords.size() ; ++i) {
			write_word(dewords[i]);
		}
		if (!NUM_WORDS) write_word("");
		// write text
		for (size_t i = 0 ; i < text.size() ; ++i) {
			write_int(words[text[i]]);
		}
	} else {
		// read table
		int n = NUM_WORDS ? read_int(cstr) : INT_MAX;
		for (int i = 0 ; i < n ; ++i) {
			dewords.push_back( read_word(cstr) );
//		cerr << "word: [" << dewords[i] << "]" << endl;
			if (dewords.back().empty()) break;
		}
		// decompres
		while (*cstr) {
			int nr = read_int(cstr);
			cout << dewords[nr];
		}
	}
	cout << endl;
	return 0;
}
