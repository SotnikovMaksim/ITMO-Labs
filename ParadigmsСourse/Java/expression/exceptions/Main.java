package expression.exceptions;

import expression.TripleExpression;

public class Main {
    public static void main(String[] args) {
        ExpressionParser p = new ExpressionParser();
        TripleExpression tp = p.parse("10 * 2");
        System.out.println(tp.toString());
    }
}
