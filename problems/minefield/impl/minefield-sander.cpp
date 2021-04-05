#include <cstdio>
#include <cmath>
#include <algorithm>
using namespace std;

#define BLAST_RADIUS 2
#define EPS          1e-6
#define DEBUG        0
#define MORE_DEBUG   0

// Comment by twan: Use doubles, not floats!

struct range {
  double start,end;
  bool operator<(const range& other) const {
    return start < other.start;
  }
};

int r,n,nmr,x[1000000],y[1000000];
range ranges[1000000];
int nrange; // number of ranges, can be > n

bool makeranges() {
  nrange = 0;
  for(int i=0;i<n;++i) {
    double d = sqrt(x[i]*x[i]+y[i]*y[i]), rd = BLAST_RADIUS / d;
    if(d < BLAST_RADIUS - EPS) {
      fprintf(stderr,"The spec promisses that this never happens\n");
      return true; // boom! dead!
    }
    double dangle = asin(rd);
    double midangle = atan2(y[i],x[i]);
    int j = nrange++;
    ranges[j].start = midangle - dangle;
    ranges[j].end   = midangle + dangle;
    if(ranges[j].start < -EPS) {
      ranges[j].start += 2*M_PI;
      ranges[j].end   += 2*M_PI;
    }
    // if the range overlaps 0, then create TWO ranges
    // This part added by Twan
    if (ranges[j].end > 2*M_PI+EPS) {
      if (MORE_DEBUG) printf("overlap zero: [%f;%f]\n", ranges[j].start*180/M_PI, ranges[j].end*180/M_PI);
      nrange++;
      ranges[j+1].start = ranges[j].start - 2*M_PI;
      ranges[j+1].end   = ranges[j].end   - 2*M_PI;
    }
  }
  sort(ranges, ranges+nrange);
  return false;
}
bool mergeranges() {
  if(DEBUG) {
   printf("BEFORE MERGE : N=%d\n",nrange);
   for(int i=0;i<nrange;++i) printf("Range %d: %f - %f\n",i,ranges[i].start*(180/M_PI),ranges[i].end*(180/M_PI));
  }
  nmr = 1; // # of merged ranges
  for(int i=1;i<nrange;++i) {
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
  #ifdef UNSAFE_IDIOT
    while(ranges[merge_upto].start+2*M_PI < ranges[nmr - 1].end - EPS) merge_upto++; // merge first(s), last?
  #else
    while(merge_upto < nmr && ranges[merge_upto].start+2*M_PI < ranges[nmr - 1].end - EPS) merge_upto++; // merge first(s), last?
  #endif

  if(nmr==1 && ranges[0].end - ranges[0].start > 2*M_PI + EPS) return true;

  if(merge_upto > 0) { 
   ranges[nmr-1].end = ranges[merge_upto-1].end + 2*M_PI; // always end > start, start < 2pi, and ranges[0].start > 0
   copy(ranges+merge_upto,ranges+nmr,ranges);
   if (merge_upto > nmr) {
    fprintf(stderr,"I AM AN IDIOT, nmr=%d merge_upto=%d\n",nmr,merge_upto);
   }
   nmr -= merge_upto;
  }
  if(DEBUG) {
   printf("AFTER FL : NMR=%d   \n",nmr);
   for(int i=0;i<nmr;++i) printf("Range %d: %f - %f\n",i,ranges[i].start*(180/M_PI),ranges[i].end*(180/M_PI));
  }
  return (nmr==1 && ranges[0].end - ranges[0].start > 2*M_PI + EPS); // dead
}

double find_largest_gap() {
  if (nmr < 0) {
    fprintf(stderr,"I AM AN IDIOT, nmr=%d\n",nmr);
  }
  double bestgap=-1, bestangle = 0;
  for(int i=0;i<nmr-1;++i) {
    double gap = ranges[i+1].start - ranges[i].end;
    if (MORE_DEBUG) {
      fprintf(stderr,"found a gap: %f   at [%f;%f]\n",gap,ranges[i].end * 180/M_PI, ranges[i+1].start * 180/M_PI);
    }
    if(gap > bestgap) {
      bestgap = gap; 
      bestangle = (ranges[i+1].start + ranges[i].end) / 2;
    }
  }
  double gap = ranges[0].start - (ranges[nmr-1].end - 2*M_PI);  // ?
  if (MORE_DEBUG) {
    fprintf(stderr,"final   gap: %f   at [%f;%f]\n",gap,(ranges[nmr-1].end - 2*M_PI) * 180/M_PI, ranges[0].start * 180/M_PI);
  }
  if(gap > bestgap) {
    bestgap = gap; 
    bestangle = (ranges[0].start + (ranges[nmr-1].end - 2*M_PI)) / 2;
  }

  return bestangle;
}

int main() {
 FILE* in = stdin;
 //FILE* in = fopen("testdata.in","rw");
 fscanf(in,"%d",&r);
 while(r--) {
   fscanf(in,"%d",&n);
   for(int i=0;i<n;++i) fscanf(in,"%d %d",&x[i],&y[i]);
   if(makeranges() || mergeranges()) puts("dead");
   else {
     double gap = find_largest_gap();
     if (gap < 0) gap += 2*M_PI;
     printf("%.10f\n", gap * (180/M_PI) );
   }
 }


}
