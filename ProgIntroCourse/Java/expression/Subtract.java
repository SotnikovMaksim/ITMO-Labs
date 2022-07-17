package expression;


public class Subtract extends BinaryOperation {
    public Subtract(Common firstExp, Common secondExp) {
        super(firstExp, secondExp, "-");
    }

    @Override
    protected int eval(int firstEval, int secondEval) {
        return firstEval - secondEval;
    }
}
