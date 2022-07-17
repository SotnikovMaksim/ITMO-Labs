package expression.exceptions;

import expression.TripleExpression;

public class CheckedMax extends CheckedBinaryOperation{
    public CheckedMax(TripleExpression firstExp, TripleExpression secondExp) {
        super(firstExp, secondExp, "max");
    }

    @Override
    public int eval(int a, int b) {
        return a >= b ? a : b;
    }
}