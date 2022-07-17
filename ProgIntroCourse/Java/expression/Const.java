package expression;

public class Const implements Common {
    public final int value;

    public Const(int value) {
        this.value = value;
    }

    @Override
    public int evaluate(int value) {
        return this.value;
    }

    public int evaluate(int xValue, int yValue, int zValue) {
        return this.value;
    }

    public String toString() {
        return Integer.toString(value);
    }

    public int hashCode() {
        return Integer.hashCode(value);
    }

    public boolean equals(Object exp) {
        if (exp == null) {
            return false;
        }
        if (exp.getClass() != this.getClass()) {
            return false;
        }
        return this.hashCode() == exp.hashCode();
    }
}
