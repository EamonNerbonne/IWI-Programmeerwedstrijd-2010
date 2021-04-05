import java.io.*;
import java.lang.*;
import java.util.*;


// strategy: optimal, cleaned


class State {
	public int position;
	public long load;
	public int nrstacks;
	public long stackheight;
	public long[] nrboxes = null;
	public int start;
	public int end;
		
	public State(int n,long[] box) {
		load = 0;
		position = 0;
		start = 0;
		nrstacks = n;
		long nrboxestotal = 0;
		nrboxes = new long[nrstacks];
		for (int i=0;i<nrstacks;++i) {
			nrboxes[i] = box[i];
			nrboxestotal += box[i];
		}
		stackheight = nrboxestotal/nrstacks;
		end = lastIrregularity();
	}
	
	public State(State s) {
		load = s.load;
		position = s.position;
		start = s.start;
		position = start;
		nrstacks = s.nrstacks;
		stackheight = s.stackheight;
		nrboxes = new long[nrstacks];
		for (int i=0;i<nrstacks;++i) nrboxes[i] = s.nrboxes[i];
		end=s.end;
	}
	
	public int max(int a, int b) {
		if (b > a)
			return b;
		return a;
	}

	public int nextIrregularity(int pos) {
		// returns -1 if not found, otherwise index
		int p = pos;
		while ((p < nrstacks) && (nrboxes[p] == stackheight)) ++p;
		if (p == nrstacks) return -1;
		return p;
	}
	
	public long allNeeded() {
		// assume that for start <= x <= position, nrboxes[x] <= stackheight
		long needed = 0;
		for (int i = start;i<position;++i)
			needed+= stackheight-nrboxes[i];
		return needed;
	}
	
	public int firstIrregularity() {
		// has to be one
		int p = start;
		while (nrboxes[p] == stackheight) ++p;
		return p;
	}
	
	public int lastIrregularity() {
		// doesn't have to be one
		// if not then return position
		int e = nrstacks-1;
		while ((e > position) && (nrboxes[e] == stackheight))
			--e;
		return e;
	}
	
	
	public long goUntilFirstPositiveLoad() {
		long dif = 0;
		long allneeded = 0;
		while (position <= end) {
			dif = nrboxes[position]-stackheight;
			if (dif > 0) {
				load+=dif;
				nrboxes[position]=stackheight;
				if ((allneeded > 0) && (load >= allneeded)) {
					return allneeded;
				}
			} else if (load+dif >= 0) { // this includes dif == 0, since load >= 0
				// can fill it up
				load+=dif;
				nrboxes[position]=stackheight;
			} else {
				// try to fill it up
				nrboxes[position]+=load;
				dif+=load;
				allneeded-=dif;
				load=0;
			}
			if (position < end) {
				position++;
			} else {
				return allneeded;
			}
		}
		return allneeded;
	}

	public long minRoute() {
		long stepstaken = 0;
		position = start;
		int oldposition = position;
		long allneeded = goUntilFirstPositiveLoad();
		stepstaken+=position-oldposition;
		if ((allneeded > 0) && (load >= allneeded)) {
			int first = firstIrregularity();
			int aa = position-first;
			int bb = lastIrregularity()-position;
			State s = new State(this);
			s.start = position;
			s.load -= allneeded;
			long mrb = s.minRoute();
			if (aa+mrb < 2*bb) {
				stepstaken += 2*(position - first);
				for (int p = first;p<position;++p) {
					long mdif = stackheight-nrboxes[p];
					if (mdif > 0) {
						load-=mdif;
						nrboxes[p]+=mdif;
					}
				}
				return stepstaken+mrb;
			} else {
				return stepstaken+(2*bb)+aa;
			}
		}
		if (nextIrregularity(start) != -1) {
			stepstaken+=position-firstIrregularity();
		}
		return stepstaken;
	}
}

class Forklift_ando extends Sjabloon_ando {} // for Makefile
class Main extends Sjabloon_ando {} // for DOMJudge
class Sjabloon_ando {

	static Scanner sc = new Scanner(System.in);

	private static long leesInt() throws IOException {
		return sc.nextLong();
	}

	public static void main(String argv[]) throws IOException {
		long runCnt = leesInt();
		for (long i = 0;i < runCnt; i++) {
			run();
		}
	}

	private static void run() throws IOException {
		int n = (int)leesInt();
		long box[] = new long[n];

		for (int i=0;i<n;++i) {
			box[i] = leesInt();
		}

		State s = new State(n,box);

		System.out.println(2*s.minRoute());
		//System.out.println(s.minRoute());
	}

}
