package expression.exceptions;

import expression.TripleExpression;

public class CheckedNegate implements TripleExpression {
    private final TripleExpression expression;

    public CheckedNegate(TripleExpression expression) {
        this.expression = expression;
    }

    public String toString() {
        return "-(" + expression.toString() + ")";
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int result = expression.evaluate(x, y, z);
        if (result == Integer.MIN_VALUE) {
            throw new OverflowException("negate: " + result);
        }
        return -result;
    }
}
