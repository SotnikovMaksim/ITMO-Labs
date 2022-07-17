package expression;


public class Divide extends BinaryOperation {
    public Divide(Common firstExp, Common secondExp) {
        super(firstExp, secondExp, "/");
    }

    @Override
    protected int eval(int firstEval, int secondEval) {
        return firstEval / secondEval;
    }
}
