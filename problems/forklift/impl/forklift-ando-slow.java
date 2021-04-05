import java.io.*;
import java.lang.*;
import java.util.*;


// too slow on test data, but correct answers


class State {
	public static final int FORWARD = 1;
	public static final int BACKWARD = 2;
		
	public long stepstaken;
	public int position;
	public long load;
	public int nrstacks;
	public long stackheight;
	public int direction;
	public long[] nrboxes = null;

	public State(int n,long[] box) {
		direction = FORWARD;
		stepstaken = 0;
		load = 0;
		position = 0;
		nrstacks = n;
		long nrboxestotal = 0;
		nrboxes = new long[nrstacks];
		for (int i=0;i<nrstacks;++i) {
			nrboxes[i] = box[i];
			nrboxestotal += box[i];
		}
		if ((nrboxestotal % nrstacks) != 0) {
			System.out.println("ERROR: boxes cannot be spread evenly over n");
		}
		stackheight = nrboxestotal/nrstacks;
	}
	
	public State(State s) {
		stepstaken = s.stepstaken;
		position = s.position;
		load = s.load;
		nrstacks = s.nrstacks;
		stackheight = s.stackheight;
		direction = s.direction;
		nrboxes = new long[nrstacks];
		for (int i=0;i<nrstacks;++i) {
			nrboxes[i] = s.nrboxes[i];
		}
	}

	public boolean done() {
		for (int i=0;i<nrstacks;++i)
			if (nrboxes[i] != stackheight)
				return false;
		return true;
	}

	public int nextIrregularity() {
		return nextIrregularity(direction);
	}
	
	public int nextIrregularity(int dir) {
		return nextIrregularity(dir,position);
	}
	
	public int nextIrregularity(int dir,int pos) {
		// returns -1 if not found, otherwise index
		int p = pos;
		if (dir == FORWARD) {
			while ((p < nrstacks) && (nrboxes[p] == stackheight)) {
				++p;
			}
			if (p == nrstacks) {
				return -1;
			} else {
				return p;
			}
		} else { // dir == BACKWARD
			if (dir != BACKWARD) { System.out.println("ERROR: not backward"); }
			while ((p >= 0) && (nrboxes[p] == stackheight)) {
				--p;
			}
			return p;
		}
	}
	
	
	public long allNeeded() {
		// assume that for 0 <= x <= position, nrboxes[x] <= stackheight
		long needed = 0;
		for (int i =0;i<position;++i) {
			if (nrboxes[i] < stackheight) {
				needed+= stackheight-nrboxes[i];
			} else if (nrboxes[i] > stackheight) { // ELSEIF > ---> ERROR
				System.out.println("ERROR: nrboxes i bigger than stackheight");
			}
		}
		return needed;
	}
	
	public int firstIrregularity() {
		// has to be one
		int p = 0;
		while (nrboxes[p] == stackheight) { // elseif bigger, then ERROR
			++p;
		}
		if (nrboxes[p] > stackheight) { System.out.println("ERROR: nrboxes p bigger than stackheight"); }
		return p;
	}
	
	public long min(long a, long b) {
		if (b < a) {
			return b;
		}
		return a;
	}
	
	public String toString() {
		String str = "";
		for (int i = 0;i < nrstacks;++i) {
			str+= "--";
		}
		str+="\n";
		for (int i = 0;i < nrstacks;++i) { 
			str+=nrboxes[i]+" ";
		}
		str+="\n";
		for (int i = 0;i < position;++i) {
			str+="  ";
		}
		str+=".\n";
		for (int i = 0;i < nrstacks;++i) {
			str+= "--";
		}
		str+="\n";
		return str;
	}
	public long minRoute() {
		
		//System.out.println(toString());
		long minfullroute = 3*nrstacks;
		
		// do heenweg
		if (direction == FORWARD) {
			int nextpos = nextIrregularity();
			while (nextpos != -1) {
				//System.out.println("in outer loop, nextpos = "+nextpos+", position = "+position);
				stepstaken+=nextpos-position;
				position=nextpos;
				
				long dif = nrboxes[position]-stackheight;
				// if dif > 0 then we need to pick up boxes
				if (dif > 0) {
					load+=dif;
					nrboxes[position]-=dif; // should now be stackheight
					if (nrboxes[position] != stackheight) { System.out.println("ERROR: nrboxes position is not stackheight 1"); }
					
					// NOW CHOOSE TO GO BACKWARD
					long allneeded = allNeeded();
					if ((allneeded > 0) && (load >= allneeded)) {
						// we can go back or not.
						// in the recursive call we just go forward,
						// here we go back.
						State s = new State(this);
						minfullroute = min(minfullroute,s.minRoute());
						// now go back
						int first = firstIrregularity();
						stepstaken += 2*(position - first); // error if <= 0
						for (int p = first;p<position;++p) {
							long mdif = stackheight-nrboxes[p];
							if (mdif > 0) { // if < then ERROR
								load-=mdif;
								nrboxes[p]+=mdif; // should now be stackheight
								if (nrboxes[p] != stackheight) { System.out.println("ERROR: nrboxes p is not stackheight 2"); }
								if (load < 0) { System.out.println("ERROR: load is negative"); }
							} else if (mdif < 0) { System.out.println("ERROR: mdif smaller than 0"); }
							
						}
					}
				} else { // dif < 0
					if (dif == 0) { System.out.println("ERROR: dif is zero"); }
					if (load+dif >= 0) {
						// we can fill it up
						// (and i think in the optimal solution we always should)
						load+=dif;
						nrboxes[position]-=dif; // should now be stackheight
						if (nrboxes[position] != stackheight) { System.out.println("ERROR: nrboxes position is not stackheight 3"); }
					} else {
						// can't fill hole completely, try to fill it up
						// with what we have
						// (is i think the optimal tactic)
						nrboxes[position]+=load;
						dif+=load;
						load = 0;
					}
				}
				
				// all the way at the end, say
				nextpos = nextIrregularity(direction,position+1);
			}
		}
		
		direction = BACKWARD;
		// here we assume you already returned,
		// so it is smooth sailing until first irregularity;
		if (nextIrregularity() != -1) { // cannot be position
			if (nextIrregularity() == position) { System.out.println("ERROR: nextIrregularity is position"); }
			stepstaken+=position-firstIrregularity();
		}
		return min(minfullroute,stepstaken);
	}

}

class Forklift_ando extends Sjabloon_ando {} // for Makefile
class Main extends Sjabloon_ando {} // for DOMJudge
class Sjabloon_ando {

	static Scanner sc = new Scanner(System.in);

	private static long leesInt() throws IOException {
		return sc.nextLong();
	}

	private static String leesRegel() throws IOException {
		return sc.next(); // reads a word, not a line
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
