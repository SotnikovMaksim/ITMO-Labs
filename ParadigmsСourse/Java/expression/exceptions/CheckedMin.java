package expression.exceptions;

import expression.TripleExpression;

public class CheckedMin extends CheckedBinaryOperation {
    public CheckedMin(TripleExpression firstExp, TripleExpression secondExp) {
        super(firstExp, secondExp, "min");
    }

    @Override
    public int eval(int a, int b) {
        return a <= b ? a : b;
    }
}
