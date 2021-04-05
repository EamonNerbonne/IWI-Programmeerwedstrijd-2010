import java.io.*;
import java.lang.*;
import java.util.*;


// strategy: optimal, cleaned


class State {
	public long[] nrboxes = null;
	public int end;
	public static long[] three = new long [3];

	public State(int n,long[] box) {
		long nrboxestotal = 0;
		nrboxes = new long[n];
		for (int i=0;i<n;++i) {
			nrboxes[i] = box[i];
			nrboxestotal += box[i];
		}
		long stackheight = nrboxestotal/n;
		end = 0;
		for (int i=0;i<n;++i) {
			nrboxes[i] -= stackheight;
			if (nrboxes[i] != 0) {
				end = i;
			}
		}
	}

	public void goUntilFirstPositiveLoad(int position, long load) {
		int minindex = -1;
		while (position <= end) {
			load+=nrboxes[position];
			if ((load >= 0) && (minindex != -1)) {
				three[0] = minindex; three[1] = position; three[2] = load; return;
			}
			if ((load < 0) && (minindex == -1)) {
				minindex = position;
			}
			if (position < end) {
				position++;
			} else {
				three[0] = minindex; three[1] = position; three[2] = load; return;
			}
		}
		three[0] = minindex; three[1] = position; three[2] = load; return;
	}

	public long minRoute(int start, long load) {
		goUntilFirstPositiveLoad(start,load);
		int minindex = (int)(three[0]);
		int position = (int)(three[1]);
		load = three[2];
		long stepstaken=position-start;
		if (position != end) {
			int dfirst = position-minindex;
			int dlast = end-position;
			long mrb = minRoute(position+1,load);
			if (dfirst+mrb < 2*dlast) {
				return stepstaken+(2*dfirst)+1+mrb;
			} else {
				return stepstaken+(2*dlast)+dfirst;
			}
		}
		if (minindex != -1) {
			stepstaken+=position-minindex;
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

		System.out.println(2*s.minRoute(0,0));
		//System.out.println(s.minRoute(0,0));
	}

}
