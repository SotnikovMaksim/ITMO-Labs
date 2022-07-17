package expression;


public class Max extends BinaryOperation {
    public Max(Common first, Common second) {
        super(first, second, "v");
    }

    @Override
    public int eval(int  a, int b) {
        return a >= b ? a : b;
    }
}
