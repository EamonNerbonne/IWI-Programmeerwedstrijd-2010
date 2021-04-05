import java.io.*;
import java.lang.*;
import java.util.*;

class MyPair {
	public String s = null; //string
	public String t = null; //type

	public MyPair(String s, String t) {
		this.s = s;
		this.t = t;
	}

	public void addParentheses() {
		s = "("+s+")";
		t = "unit";
	}
}

class MyStack {
	public Stack<MyPair> ms = new Stack<MyPair>();

	public void push(MyPair s) {
		ms.push(s);
	}
	public MyPair peek() {
		if (ms.empty())
			System.out.println("ERROR: peek: stack is empty.");
		return ms.peek();
	}
	public MyPair pop() {
		if (ms.empty())
			System.out.println("ERROR: pop: stack is empty.");
		return ms.pop();
	}
	public boolean empty() {
		return ms.empty();
	}
}

class Main extends Sjabloon_ando {} // for DOMJudge
class Sjabloon_ando {

	static Scanner sc = new Scanner(System.in);
	static MyStack ms = null;

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
		ms = new MyStack();
		for (int x=0;x<n;++x) readAssemblyLine();
		if (!ms.empty())
			System.out.println("ERROR: stack is not empty at end of program.");
	}

	private static void readAssemblyLine() {
		String instruction = sc.next();
		if (instruction.equals("iload")) {
			doIload();
		} else if (instruction.equals("istore")) {
			doIstore();
		} else if (instruction.equals("iconst")) {
			doIconst();
		} else if (instruction.equals("iadd")) {
			doIadd();
		} else if (instruction.equals("isub")) {
			doIsub();
		} else if (instruction.equals("ineg")) {
			doIneg();
		} else if (instruction.equals("imul")) {
			doImul();
		} else if (instruction.equals("idiv")) {
			doIdiv();
		} else {
			System.out.println("ERROR: unrecognized instruction: "+instruction);
		}
	}

	private static void doIload() {
		String var = sc.next();
		ms.push(new MyPair(var,"unit"));
	}
	private static void doIstore() {
		String var = sc.next();
		MyPair plusexp = ms.pop();
		// end of statement
		// don't need to put brackets areound this statement.
		System.out.println(var+" = "+plusexp.s+";");
	}
	private static void doIconst() {
		Integer num = sc.nextInt();
		ms.push(new MyPair(num.toString(),"unit"));
	}
	private static void doIadd() {
		//add parentheses for expressions only if they are a (non-closed) timesexp
		MyPair a = ms.pop();
		MyPair b = ms.pop();
		ms.push(new MyPair(b.s+" + "+a.s,"plusexp"));
	}
	private static void doIsub() {
		MyPair a = ms.pop();
		if (a.t.equals("plusexp")) {
			a.addParentheses();
		} 
		MyPair b = ms.pop();
		ms.push(new MyPair(b.s+" - "+a.s,"plusexp"));
	}
	private static void doIneg() {
		MyPair a = ms.pop();
		if (!a.t.equals("unit")) {
			a.addParentheses();
		}
		ms.push(new MyPair("-"+a.s,"negexp"));
	}
	private static void doImul() {
		MyPair a = ms.pop();
		if (a.t.equals("plusexp")) {
			a.addParentheses();
		}
		MyPair b = ms.pop();
		if (b.t.equals("plusexp")) {
			b.addParentheses();
		}
		ms.push(new MyPair(b.s+" * "+a.s,"timesexp"));
	}
	private static void doIdiv() {
		MyPair a = ms.pop();
		if (a.t.equals("plusexp") || a.t.equals("timesexp")) {
			a.addParentheses();
		}
		MyPair b = ms.pop();
		if (b.t.equals("plusexp")) {
			b.addParentheses();
		}
		ms.push(new MyPair(b.s+" / "+a.s,"timesexp"));
	}
}
