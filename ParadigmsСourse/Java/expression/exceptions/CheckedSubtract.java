package expression.exceptions;

import expression.TripleExpression;

public class CheckedSubtract extends CheckedBinaryOperation {
    public CheckedSubtract(TripleExpression firstExp, TripleExpression secondExp) {
        super(firstExp, secondExp, "-");
    }

    @Override
    protected int eval(int firstEval, int secondEval) {
        if (secondEval >= 0 && MIN_INT + secondEval > firstEval || secondEval < 0 && MAX_INT + secondEval < firstEval) {
            throw new OverflowException("subtract: " + firstEval + " " + secondEval);
        }
        return firstEval - secondEval;
    }
}
