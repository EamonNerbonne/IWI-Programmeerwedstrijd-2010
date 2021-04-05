// 46 minuten

#include <limits.h>
#include <stdio.h>
#include <queue>

int nr,nc;
int maze[1000][1000];

struct Pos {
    Pos(int r, int c) : r(r), c(c) {}
    int r, c;
};
typedef Pos Dir;

struct Hit {
    Hit(const Pos& p, int n) : p(p), n(n) {}
    Pos p;
    int n;
};

Pos operator+(const Pos& p, const Dir& d) { return Pos(p.r+d.r,p.c+d.c); }

bool isPassable(const Pos& p) {
    return 0<=p.r && p.r<nr && 0<=p.c && p.c<nc && maze[p.r][p.c]>=0;
}

Pos find_hit(Pos p, const Dir& d1, const Dir& d2) {
    while(true) {
        if (isPassable(p+d1)) {
            p = p + d1;
        } else if (isPassable(p+d2)) {
            p = p + d2;
        } else {
            break;
        }
    }
    return p;
}

void run() {
    // Input
    int sr=-1, sc=-1, er=-1, ec=-1;
    scanf("%d %d",&nr,&nc);
    char lineformat[20];
    sprintf(lineformat,"%%%ds\n",nc);
    for(int i=0; i<nr; i++) {
        char line[1001];
        scanf(lineformat,line);
        for(int j=0; j<nc; j++) {
            if (line[j]=='A') {
                sr = i;
                sc = j;
                maze[i][j] = INT_MAX;
            } else if (line[j]=='B') {
                er = i;
                ec = j;
                maze[i][j] = INT_MAX;
            } else if (line[j]=='X') {
                maze[i][j] = -1;
            } else {
                maze[i][j] = INT_MAX;
            }
        }
    }
    if (sr==-1 || sc==-1 || er==-1 || ec==-1) {
        printf("error\n");
        return;
    }

    // Flood-fill
    std::queue<Hit> q;
    q.push(Hit(Pos(sr,sc),0));
    while(!q.empty()) {
        Hit h(q.front()); q.pop();
        if (h.n<maze[h.p.r][h.p.c]) {
            maze[h.p.r][h.p.c] = h.n;
            q.push(Hit(find_hit(h.p,Dir( 1, 0),Dir( 0, 1)),h.n+1));
            q.push(Hit(find_hit(h.p,Dir( 1, 0),Dir( 0,-1)),h.n+1));
            q.push(Hit(find_hit(h.p,Dir(-1, 0),Dir( 0, 1)),h.n+1));
            q.push(Hit(find_hit(h.p,Dir(-1, 0),Dir( 0,-1)),h.n+1));
            q.push(Hit(find_hit(h.p,Dir( 0, 1),Dir( 1, 0)),h.n+1));
            q.push(Hit(find_hit(h.p,Dir( 0, 1),Dir(-1, 0)),h.n+1));
            q.push(Hit(find_hit(h.p,Dir( 0,-1),Dir( 1, 0)),h.n+1));
            q.push(Hit(find_hit(h.p,Dir( 0,-1),Dir(-1, 0)),h.n+1));
        }
    }

    // Output
    if (maze[er][ec]==INT_MAX) {
        printf("no solution\n");
    } else {
        printf("%d\n",maze[er][ec]);
    }
}

int main() {
    int runs;
    scanf("%d",&runs);
    while(runs-->0) run();
    return 0;
}
