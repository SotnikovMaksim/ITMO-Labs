package expression;

public class Multiply extends BinaryOperation {
    public Multiply(Common firstExp, Common secondExp) {
        super(firstExp, secondExp, "*");
    }

    @Override
    protected int eval(int firstEval, int secondEval) {
        return firstEval * secondEval;
    }
}
