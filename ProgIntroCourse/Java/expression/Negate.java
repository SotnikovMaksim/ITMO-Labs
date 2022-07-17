package expression;


public class Negate implements Common {
    private final Common expression;

    public Negate(TripleExpression expression) {
        this.expression = (Common) expression;
    }

    public String toString() {
        return "-(" + expression.toString() + ")";
    }
    @Override
    public int evaluate(int x, int y, int z) {
        return -(expression.evaluate(x, y, z));
    }

    @Override
    public int evaluate(int x) {
        return -(expression.evaluate(x));
    }
}