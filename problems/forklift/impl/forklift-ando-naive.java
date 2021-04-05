import java.io.*;
import java.lang.*;
import java.util.*;


// strategy: from left to right and back, stopping as soon
// as we're done.


class State {
	public long stepstaken;
	public int position;
	public long load;
	public int nrstacks;
	public long stackheight;
	public long[] nrboxes = null;

	public State(int n,long[] box) {
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

	public boolean done() {
		for (int i=0;i<nrstacks;++i)
			if (nrboxes[i] != stackheight)
				return false;
		return true;
	}

	public void goRight() {
		++position;
		++stepstaken;
	}

	public void goLeft() {
		--position;
		++stepstaken;
	}

	public int findEnd() {
		int e = nrstacks-1;
		while ((e >= 0) && (nrboxes[e] == stackheight))
			--e;
		return e;
	}

	public int findStart() {
		int e = 0;
		while ((e < nrstacks) && (nrboxes[e] == stackheight))
			++e;
		return e;
	}

	public long minRoute() {
		long needs = 0;
		long realneeds = 0;
		int needspos = -1;
		int end = findEnd();
		if (end == -1) { return stepstaken; }
		while (position <= end) {
			long dif = nrboxes[position]-stackheight;
			// if dif > 0 then we need to pick up boxes
			if (dif > 0) {
				load+=dif;
				nrboxes[position]-=dif; // should now be stackheight
				if (position < end) goRight();
			} else if (dif < 0) {
				if (load+dif >= 0) { // we can fill it up
					load+=dif;
					nrboxes[position]-=dif; // should now be stackheight
					if (position < end) goRight();
					else break;
				} else {
					// can't fill hole completely, try to fill it up
					// with what we have
					nrboxes[position]+=load;
					dif+=load;
					load = 0;
					if (position < end) goRight();
					else break;
				}
			} else { // dif == 0
				if (position < end) goRight();
				else break;
			}
		}
		int start = findStart();
		if (start == nrstacks) {
			//System.out.println("yes");
			return stepstaken;
		}
		stepstaken+=position-start;
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
