package expression.generic;

public class GenericDouble extends GenericADT<Double> {
    @Override
    protected Double add(Double x, Double y) {
        return x + y;
    }

    @Override
    protected Double sub(Double x, Double y) {
        return x - y;
    }

    @Override
    protected Double mult(Double x, Double y) {
        return x * y;
    }

    @Override
    protected Double div(Double x, Double y) {
        return x / y;
    }

    @Override
    protected Double negate(Double x) {
        return -x;
    }

    @Override
    protected Double parse(int x) {
        return Double.parseDouble(String.valueOf(x));
    }

    @Override
    protected Double max(Double x, Double y) {
        return Double.max(x, y);
    }

    @Override
    protected Double min(Double x, Double y) {
        return Double.min(x, y);
    }

    @Override
    protected int count(Double x) {
        return Long.bitCount(Double.doubleToLongBits(x));
    }
}
