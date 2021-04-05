import java.util.*;
class Solution extends TiltmazeTwan {}
class TiltmazeTwan {
	static Scanner sc = new Scanner(System.in);
	public static void main(String[] args) {
		int runs = sc.nextInt();
		while (runs --> 0) new TiltmazeTwan();
	}
	
	class Point{
		int x,y;
		Point(int y,int x) { this.x = x; this.y = y; }
		boolean equals(Point p) { return this.x == p.x && this.y == p.y; }
	}
	
	int h,w;
	char[][] grid;
	Point a,b;
	
	public TiltmazeTwan() {
		readInput();
		int steps = solve(); //TODO
		if (steps == -1) {
			System.out.println("no solution");
		} else {
			System.out.println(steps);
		}
	}
	void readInput() {
		h = sc.nextInt();
		w = sc.nextInt();
		grid = new char[h][w];
		for (int y = 0 ; y < h ; ++y) {
			String s = sc.next();
			for (int x = 0 ; x < w ; ++x) {
				grid[y][x] = s.charAt(x);
				if (grid[y][x] == 'A') a = new Point(y,x);
				if (grid[y][x] == 'B') b = new Point(y,x);
			}
		}
		if (a == null) throw new RuntimeException("bork a");
		if (b == null) throw new RuntimeException("bork b");
	}
	int solve() {
		ArrayList<Point> pts = new ArrayList<Point>();
		pts.add(a);
		for (int step = 0 ; ; ++step) {
			if (pts.isEmpty()) return -1;
			ArrayList<Point> pts2 = new ArrayList<Point>();
			for (Point aa : pts) {
				if (aa.equals(b)) return step;
				add(pts2, walkTo(aa.y,aa.x,  1,0, 0, 1));
				add(pts2, walkTo(aa.y,aa.x,  1,0, 0,-1));
				add(pts2, walkTo(aa.y,aa.x, -1,0, 0, 1));
				add(pts2, walkTo(aa.y,aa.x, -1,0, 0,-1));
				add(pts2, walkTo(aa.y,aa.x, 0, 1,  1,0));
				add(pts2, walkTo(aa.y,aa.x, 0, 1, -1,0));
				add(pts2, walkTo(aa.y,aa.x, 0,-1,  1,0));
				add(pts2, walkTo(aa.y,aa.x, 0,-1, -1,0));
			}
			pts = pts2;
		}
	}
	void add(ArrayList<Point> pts, Point p) {
		if (p != null && grid[p.y][p.x] != 'A') {
			grid[p.y][p.x] = 'A';
			pts.add(p);
		}
	}
	Point walkTo(int y, int x, int dy,int dx, int dy2,int dx2) {
		while (true) {
			if (y+dy >= 0 && y+dy < h && x+dx >= 0 && x+dx < w && grid[y+dy][x+dx] != 'X') {
				y += dy; x += dx;
			} else if (y+dy2 >= 0 && y+dy2 < h && x+dx2 >= 0 && x+dx2 < w && grid[y+dy2][x+dx2] != 'X') {
				y += dy2; x += dx2;
			} else {
				return new Point(y,x);
			}
		}
	}
	
}