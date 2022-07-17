package expression;


public class Min extends BinaryOperation {
    public Min(Common first, Common second) {
        super(first, second, "v");
    }

    @Override
    public int eval(int a, int b) {
        return a <= b ? a : b;
    }
}
