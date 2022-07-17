package expression.generic;

import expression.exceptions.*;


public class GenericInt extends GenericADT<Integer> {
    private final Integer MAX_INT = Integer.MAX_VALUE;
    private final Integer MIN_INT = Integer.MIN_VALUE;

    @Override
    protected Integer add(Integer x, Integer y) {
        if (y > 0 && MAX_INT - y < x || y < 0 && MIN_INT - y > x) {
            throw new expression.exceptions.OverflowException("add: " + x + " " + y);
        }
        return x + y;
    }

    @Override
    protected Integer sub(Integer x, Integer y) {
        if (y >= 0 && MIN_INT + y > x || y < 0 && MAX_INT + y < x) {
            throw new OverflowException("subtract: " + x + " " + y);
        }
        return x - y;
    }

    @Override
    protected Integer mult(Integer x, Integer y) {
        if (x > y) {
            this.mult(y, x);
        }
        if ((x < 0 && y < 0 && x < MAX_INT / y) ||
                (x < 0 && y > 0 && x < MIN_INT / y) ||
                (x > 0 && y > 0 && x > MAX_INT / y)) {
            throw new OverflowException("multiply: " + x + " " + y);
        }
        return x * y;
    }

    @Override
    protected Integer div(Integer x, Integer y) {
        if (y == 0) {
            throw new DivisionByZeroException("Division by zero: " + x + " " + y);
        }
        if (x == MIN_INT && y == -1) {
            throw new OverflowException("divide: " + x + " " + y);
        }
        return x / y;
    }

    @Override
    public Integer negate(Integer x) {
        if (x == Integer.MIN_VALUE) throw new OverflowException("negate: " + x);
        return -x;
    }

    @Override
    protected Integer parse(int x) {
        return x;
    }

    @Override
    protected Integer max(Integer x, Integer y) {
        return Integer.max(x, y);
    }

    @Override
    protected Integer min(Integer x, Integer y) {
        return Integer.min(x, y);
    }

    @Override
    protected int count(Integer x) {
        return Integer.bitCount(x);
    }
}
