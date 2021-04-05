#include <stdio.h>
#include <string.h>

char in[1000000];
char out[2000000];

void compress() {
    char *s = in;
    char *d = out;
    while(*s) {
        if (*s=='.') {
            *d++ = '.';
            *d++ = *s++;
        } else if (strncmp(s,"the",3)==0) {
            *d++ = '.';
            *d++ = 'a';
            s += 3;
        } else if (strncmp(s,"and",3)==0) {
            *d++ = '.';
            *d++ = 'b';
            s += 3;
        } else {
            *d++ = *s++;
        }
    }
    *d = 0;
    printf("%s\n", out);
}

void decompress() {
    char *s = in;
    char *d = out;
    while(*s) {
        if (*s=='.' && *(s+1)=='.') {
            *d++ = '.';
            s += 2;
        } else if (*s=='.' && *(s+1)=='a') {
            *d++ = 't';
            *d++ = 'h';
            *d++ = 'e';
            s += 2;
        } else if (*s=='.' && *(s+1)=='b') {
            *d++ = 'a';
            *d++ = 'n';
            *d++ = 'd';
            s += 2;
        } else {
            *d++ = *s++;
        }
    }
    printf("%s\n", out);
}

int main() {
    char str[20];
    scanf("%s\n",str);
    gets(in);
    if (strcmp(str,"compress")==0) {
        compress();
    } else if (strcmp(str,"decompress")==0) {
        decompress();
    } else {
        fprintf(stderr,"Error: unrecognized mode '%s'.",str);
    }
    return 0;
}
