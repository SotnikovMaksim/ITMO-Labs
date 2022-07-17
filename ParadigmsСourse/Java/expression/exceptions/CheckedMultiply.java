package expression.exceptions;


import expression.TripleExpression;

public class CheckedMultiply extends CheckedBinaryOperation {
    public CheckedMultiply(TripleExpression firstExp, TripleExpression secondExp) {
        super(firstExp, secondExp, "*");
    }

    @Override
    protected int eval(int firstEval, int secondEval) {
        if (firstEval > secondEval) {
            this.eval(secondEval, firstEval);
        }
        if ((firstEval < 0 && secondEval < 0 && firstEval < MAX_INT / secondEval) ||
            (firstEval < 0 && secondEval > 0 && firstEval < MIN_INT / secondEval) ||
            (firstEval > 0 && secondEval > 0 && firstEval > MAX_INT / secondEval)) {
            throw new OverflowException("multiply: " + firstEval + " " + secondEval);
        }
        return firstEval * secondEval;
    }
}