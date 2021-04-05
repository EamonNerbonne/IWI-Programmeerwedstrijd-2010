#include <cstdio>
#include <cmath>
#include <algorithm>
using namespace std;

#define BLAST_RADIUS 2
#define EPS          1e-6
#define DEBUG        0

struct range {
  float start,end;
  bool operator<(const range& other) const {
    return start < other.start;
  }
};

int r,n,nmr,x[1000000],y[1000000];
range ranges[1000000];

bool makeranges() {
  for(int i=0;i<n;++i) {
    float d = sqrt(x[i]*x[i]+y[i]*y[i]), rd = BLAST_RADIUS / d;
    if(d < BLAST_RADIUS - EPS) return true; // boom! dead!
    float dangle = asin(rd);
    float midangle = atan2(y[i],x[i]); 
    ranges[i].start = midangle - dangle;
    ranges[i].end   = midangle + dangle;
    if(ranges[i].start < -EPS) {
      ranges[i].start += 2*M_PI;
      ranges[i].end   += 2*M_PI;
    }
  }
  sort(ranges, ranges+n);
  return false;
}
bool mergeranges() {
  if(DEBUG) {
   printf("BEFORE MERGE : N=%d\n",n);
   for(int i=0;i<n;++i) printf("Range %d: %f - %f\n",i,ranges[i].start*(180/M_PI),ranges[i].end*(180/M_PI));
  }
  nmr = 1; // # of merged ranges
  for(int i=1;i<n;++i) {
    if(ranges[i].start < ranges[nmr-1].end - EPS) {
      ranges[nmr-1].end = max(ranges[nmr-1].end , ranges[i].end); // merge
    }
    else {
      ranges[nmr] = ranges[i];
      nmr++;
    }
  }
  if(DEBUG) {
   printf("BEFORE FL : NMR=%d\n",nmr);
   for(int i=0;i<nmr;++i) printf("Range %d: %f - %f\n",i,ranges[i].start*(180/M_PI),ranges[i].end*(180/M_PI));
  }
  int merge_upto = 0;
  while(ranges[merge_upto].start+2*M_PI < ranges[nmr - 1].end - EPS) merge_upto++; // merge first(s), last?

  if(nmr==1 && ranges[0].end - ranges[0].start > 2*M_PI + EPS) return true;

  if(merge_upto > 0) { 
   ranges[nmr-1].end = ranges[merge_upto-1].end + 2*M_PI; // always end > start, start < 2pi, and ranges[0].start > 0
   copy(ranges+merge_upto,ranges+nmr,ranges);
   nmr -= merge_upto;
  }
  if(DEBUG) {
   printf("AFTER FL : NMR=%d   \n",nmr);
   for(int i=0;i<nmr;++i) printf("Range %d: %f - %f\n",i,ranges[i].start*(180/M_PI),ranges[i].end*(180/M_PI));
  }
  return (nmr==1 && ranges[0].end - ranges[0].start > 2*M_PI + EPS); // dead
}

float find_largest_gap() {
  float bestgap=-1, bestangle;
  for(int i=0;i<nmr-1;++i) {
    float gap = ranges[i+1].start - ranges[i].end;
    if(gap > bestgap) {
      bestgap = gap; 
      bestangle = (ranges[i+1].start + ranges[i].end) / 2;
    }
  }
  float gap = ranges[0].start - (ranges[nmr-1].end - 2*M_PI);  // ?
  if(gap > bestgap) {
    bestgap = gap; 
    bestangle = (ranges[0].start + (ranges[nmr-1].end - 2*M_PI)) / 2;
  }

  return bestangle;
}

int main() {
 scanf("%d",&r);
 while(r--) {
   scanf("%d",&n);
   for(int i=0;i<n;++i) scanf("%d %d",&x[i],&y[i]);
   if(makeranges() || mergeranges()) puts("dead");
   else printf("%f\n", find_largest_gap() * (180/M_PI) ); // wrong for wraparound
 }


}
