import static org.junit.Assert.*;

public class Test {

	@org.junit.Test
	public void testParent() {
		
	}
	
	@org.junit.Test
	public void test() {
		assertEquals(7, EvaluateExpressions.evaluate("(3+5*2) -(2 *3)"));
		assertEquals(21, EvaluateExpressions.evaluate("(3+5*2) -(2 -3) * 8"));
		assertEquals(35, EvaluateExpressions.evaluate("(3+4)*5"));
		assertEquals(17, EvaluateExpressions.evaluate("3*4+5"));
		assertEquals(60, EvaluateExpressions.evaluate("3*4*5"));
		assertEquals(23, EvaluateExpressions.evaluate("3+4*5"));
		assertEquals(-1, EvaluateExpressions.evaluate("4u8"));
		assertEquals(-1, EvaluateExpressions.evaluate("4*"));
		assertEquals(2, EvaluateExpressions.evaluate("4*3/5"));
		assertEquals(-1, EvaluateExpressions.evaluate("4*3/0"));
	}
	
	
	
}
