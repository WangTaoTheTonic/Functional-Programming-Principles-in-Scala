import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class EvaluateExpressions {

	public static int evaluate(String expr) {
		try {
			List<String> list = makeList(expr);
			List<String> suffix = transform(list);
			return compute(suffix);
		} catch (Exception e) {
			return -1;
		}
	}
	
	private static int compute(List<String> list) {
		Stack<Integer> numStack = new Stack<Integer>();
		for (int i = 0; i < list.size(); i ++) {
			String cur = list.get(i);
			if (!isOpChar(cur.charAt(0))) {
				numStack.push(Integer.valueOf(cur));
			} else {
				switch (cur) {
				case "+":
					numStack.push(numStack.pop() + numStack.pop());
					break;
				case "-":
					int a = numStack.pop();
					int b = numStack.pop();
					numStack.push(b - a);
					break;
				case "*":
					numStack.push(numStack.pop() * numStack.pop());
					break;
				case "/":
					int c = numStack.pop();
					int d = numStack.pop();
					numStack.push(d / c);
					break;
				}
			}
		}
		return numStack.pop();
	}
	
	private static List<String> transform(List<String> list) {
		Stack<String> opStack = new Stack<String>();
		List<String> ans = new ArrayList<String>();
		for (int i = 0; i < list.size(); i ++) {
			String cur = list.get(i);
			if (!isOpChar(cur.charAt(0))) {
				ans.add(cur);
			} else {
				switch (cur) {
				case "(":
					opStack.push(cur);
					break;
				case ")":
					while (!opStack.peek().equals("(")) {
						ans.add(opStack.pop());
					}
					opStack.pop();
					break;
				default:
					if (opStack.isEmpty()) {
						opStack.push(cur);
						break;
					}
					if (compare(cur, opStack.peek())) {
						opStack.push(cur);
					} else {
						while (!opStack.isEmpty() && !compare(cur, opStack.peek())) {
							ans.add(opStack.pop());
						}
						opStack.push(cur);
					}
				}
			}
		}
		while (!opStack.isEmpty()) {
			ans.add(opStack.pop());
		}
		return ans;
	}
	
	private static boolean compare(String cur, String top) {
		if (cur.equals("*") || cur.equals("/")) {
			if (top.equals("+") || top.equals("-")) return true;
		}
		if (top.equals("(")) return true;
		return false;
	}
	
	private static List<String> makeList(String expr) {
		List<String> ans = new ArrayList<String>();
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < expr.length(); i ++) {
			char tmp = expr.charAt(i);
			if (tmp == ' ') continue;
			if (isOpChar(tmp)) {
				if (!sb.toString().isEmpty()) {
					ans.add(sb.toString());
					sb = new StringBuilder();
				}
				ans.add(String.valueOf(tmp));
			} else {
				sb.append(tmp);
			}
		}
		if (!sb.toString().isEmpty()) {
			ans.add(sb.toString());
		}
		return ans;
	}
	
	private static boolean isOpChar(char c) {
		return c == '+' || c == '-' || c == '*' || c == '/' || c == '(' || c == ')';
	}

}
