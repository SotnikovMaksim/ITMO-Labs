package expression.exceptions;

import expression.TripleExpression;

import java.util.Objects;

public abstract class CheckedBinaryOperation implements TripleExpression {
    public final TripleExpression firstExp;
    public final TripleExpression secondExp;
    public final String operator;
    protected final int MAX_INT = Integer.MAX_VALUE;
    protected final int MIN_INT = Integer.MIN_VALUE;
    protected abstract int eval(int firstValue, int secondValue);

    public CheckedBinaryOperation(TripleExpression firstExp, TripleExpression secondExp, String operator) {
        this.firstExp = firstExp;
        this.secondExp = secondExp;
        this.operator = " " + operator + " ";
    }

    public String toString() {
        return "(" + firstExp.toString() + operator + secondExp.toString() + ")";
    }

    public int evaluate(int x, int y, int z) {
        return eval(firstExp.evaluate(x, y, z), secondExp.evaluate(x, y, z));
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
