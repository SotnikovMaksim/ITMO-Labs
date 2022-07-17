package expression;


import java.util.Objects;

public abstract class BinaryOperation implements Common {
    public final Common firstExp;
    public final Common secondExp;
    public final String operator;
    protected abstract int eval(int x, int y);

    public BinaryOperation(Common firstExp, Common secondExp, String operator) {
        this.firstExp = firstExp;
        this.secondExp = secondExp;
        this.operator = " " + operator + " ";
    }

    public Common getFirstExp() {
        return firstExp;
    }

    public Common getSecondExp() {
        return secondExp;
    }

    public String toString() {
        return "(" + firstExp.toString() + operator + secondExp.toString() + ")";
    }

    public int evaluate(int x, int y, int z) {
        return eval(firstExp.evaluate(x, y, z), secondExp.evaluate(x, y, z));
    }

    public int evaluate(int x) {
        return eval(firstExp.evaluate(x), secondExp.evaluate(x));
    }

    @Override
    public int hashCode() {
        return Objects.hash(firstExp.hashCode(), secondExp.hashCode(), this.getClass());
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
