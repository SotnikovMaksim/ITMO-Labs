package expression;


public class Add extends BinaryOperation {
    public Add(Common firstExp, Common secondExp) {
        super(firstExp, secondExp, "+");
    }

    @Override
    protected int eval(int firstEval, int secondEval) {
         return firstEval + secondEval;
    }
}
