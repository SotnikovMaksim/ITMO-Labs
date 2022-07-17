package expression.exceptions;

import expression.TripleExpression;

public class CheckedZeroes implements TripleExpression {
    private TripleExpression value;
    private String side;

    public CheckedZeroes(TripleExpression value, String side) {
        this.value = value;
        this.side = side;
    }

    public String toString() {
        return side + "(" + value.toString() + ")";
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int calc = value.evaluate(x, y, z);
        return side.equals("l0") ? Integer.numberOfLeadingZeros(calc) : Integer.numberOfTrailingZeros(calc);
    }
}