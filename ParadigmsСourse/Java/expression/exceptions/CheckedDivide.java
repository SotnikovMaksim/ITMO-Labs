package expression.exceptions;

import expression.TripleExpression;

public class CheckedDivide extends CheckedBinaryOperation {
    public CheckedDivide(TripleExpression firstExp, TripleExpression secondExp) {
        super(firstExp, secondExp, "/");
    }

    @Override
    protected int eval(int firstEval, int secondEval) {
        if (secondEval == 0) {
            throw new DivisionByZeroException("Division by zero: " + firstEval + " " + secondEval);
        }
        if (firstEval == MIN_INT && secondEval == -1) {
            throw new OverflowException("divide: " + firstEval + " " + secondEval);
        }
        return firstEval / secondEval;
    }
}
