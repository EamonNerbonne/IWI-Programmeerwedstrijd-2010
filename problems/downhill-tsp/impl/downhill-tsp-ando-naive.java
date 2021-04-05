import java.io.*;
import java.lang.*;
import java.util.*;

// naieve implementatie
// doet real    1m22.530s over testdata

class Coord implements Comparable {
	public int street;
	public int avenue;
	
	public Coord(int street,int avenue) {
		this.street=street;
		this.avenue=avenue;
	}
	
	public int compareTo(Object o) {
		return (street-((Coord)o).street != 0)?street-((Coord)o).street:avenue-((Coord)o).avenue;
	}
	
	public String toString() {
		return "("+street+","+avenue+")";
	}
}

class DownhillTSP_ando extends Sjabloon_ando {} // for Makefile
class Main extends Sjabloon_ando {} // for DOMJudge
class Sjabloon_ando {
	
	static Scanner sc = new Scanner(System.in);

	private static long leesInt() throws IOException {
		return sc.nextLong();
	}

	public static void main(String argv[]) throws IOException {
		long runCnt = leesInt();
		for (long i = 0;i < runCnt; i++) {
			run((int)i);
		}
	}
	
	private static long max(long a, long b) {
		return (b > a) ? b : a;
	}

	private static void run(int ii) throws IOException {
		long n = leesInt(); // number of houses paul could visit
		ArrayList<Coord> coords = new ArrayList<Coord>();
		for (int i=0;i<n;++i) {
			int street = (int)leesInt();
			int avenue = (int)leesInt();
			coords.add(new Coord(street,avenue));
		}
		Collections.sort(coords);
		long[] best = new long[(int)n];
		for (int i=0;i<n;++i) {
			best[i] = 1;
			Coord coorda = coords.get(i);
			for (int j=0;j<i;++j) {
				Coord coordb = coords.get(j);
				if (coordb.avenue <= coorda.avenue) {
					best[i] = max(best[i],best[j]+1);
				}
			}
		}
		long maxbest = 0;
		for (int i=0;i<n;++i) {
			if (best[i] > maxbest) {
				maxbest = best[i];
			}
		}
		System.out.println(maxbest);
	}
	
	private static void printArray(ArrayList<Coord> coords) {
		System.out.print("[");
		for (int i=0;i<coords.size()-1;i++) {
			System.out.print(coords.get(i)+", ");
		}
		if (coords.size() > 0) {
			System.out.print(coords.get(coords.size()-1));
		}
		System.out.println("]\n");
	}

}
