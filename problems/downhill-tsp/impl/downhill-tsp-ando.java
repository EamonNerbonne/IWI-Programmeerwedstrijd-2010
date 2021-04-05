import java.io.*;
import java.lang.*;
import java.util.*;

// niet-naieve implementatie
// doet real    0m10.923s over testdata

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
		ArrayList<Long> best = new ArrayList<Long>();
		for (int i=0;i<n;++i) {
			best.add(new Long(0));
		}
		for (int i=0;i<n;++i) {
			best.set(i,new Long(1));
			Coord coorda = coords.get(i);
			for (int j=0;j<i;++j) {
				Coord coordb = coords.get(j);
				if (coordb.avenue <= coorda.avenue) {
					best.set(i,max(best.get(i),best.get(j)+1));
				}
				
			}
			for (int j=0;j<i;++j) {
				coorda = coords.get(i);
				Coord coordb = coords.get(j);
				if ((coordb.avenue > coorda.avenue) && (best.get(i)>=best.get(j))) {
					coords.remove(j);
					best.remove(j);
					i--;
					j--;
					n--;
				}
			}
		}
		long maxbest = 0;
		for (int i=0;i<n;++i) {
			if (best.get(i) > maxbest) {
				maxbest = best.get(i);
			}
		}
		System.out.println(maxbest);
	}
}
