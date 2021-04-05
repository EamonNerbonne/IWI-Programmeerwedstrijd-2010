#include <stdio.h>
#include <string.h>

const char*const words[] = {
    " the "," and "," this "," that ","ing "," to "," is "," of "," on "," in "," one "," two "
};

char in[2000002];
char out[2000002];

void compress() {
    char *s = in;
    char *d = out;
    while(*s) {
        if (*s=='.') {
            *d++ = '.';
            *d++ = *s++;
        } else {
            bool subst = false;
            for(int i=0; i<sizeof(words)/sizeof(*words); i++) {
                if (strncmp(s,words[i],strlen(words[i]))==0) {
                    *d++ = '.';
                    *d++ = (char)i+'a';
                    s += strlen(words[i]);
                    subst = true;
                    break;
                }
            }
            if (!subst) *d++ = *s++;
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
        } else if (*s=='.') {
            strcpy(d,words[*(s+1)-'a']);
            d += strlen(words[*(s+1)-'a']);
            s += 2;
        } else {
            *d++ = *s++;
        }
    }
    printf("%s\n", out);
}

int main() {
    char str[20];
    gets(str);
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
