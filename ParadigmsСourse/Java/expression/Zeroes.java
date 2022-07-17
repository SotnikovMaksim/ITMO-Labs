package expression;

public class Zeroes implements Common {
    private final Common value;
    private final String side;

    public Zeroes(TripleExpression value, String side) {
        this.value = (Common) value;
        this.side = side;
    }

    public String toString() {
        return side + "(" + value.toString() + ")";
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int calc = value.evaluate(x, y, z);
        return side.equals("l0") ? Integer.numberOfLeadingZeros(calc) : Integer.numberOfTrailingZeros(calc);
    }

    @Override
    public int evaluate(int x) {
        int calc = value.evaluate(x);
        return side.equals("l0") ? Integer.numberOfLeadingZeros(calc) : Integer.numberOfTrailingZeros(calc);
    }
}