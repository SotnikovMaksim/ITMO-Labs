package expression.generic;

import java.math.BigInteger;
import expression.exceptions.*;

public class GenericBigInt extends GenericADT<BigInteger> {
    @Override
    protected BigInteger add(BigInteger x, BigInteger y) {
        return x.add(y);
    }

    @Override
    protected BigInteger sub(BigInteger x, BigInteger y) {
        return x.subtract(y);
    }

    @Override
    protected BigInteger mult(BigInteger x, BigInteger y) {
        return x.multiply(y);
    }

    @Override
    protected BigInteger div(BigInteger x, BigInteger y) {
        if (y.compareTo(new BigInteger(String.valueOf(0))) == 0) {
            throw new DivisionByZeroException("Division by zero: " + x + " " + y);
        }
        return x.divide(y);
    }

    @Override
    protected BigInteger negate(BigInteger x) {
        return x.negate();
    }

    @Override
    protected BigInteger parse(int x) {
        return new BigInteger(String.valueOf(x));
    }

    @Override
    protected BigInteger max(BigInteger x, BigInteger y) {
        return x.max(y);
    }

    @Override
    protected BigInteger min(BigInteger x, BigInteger y) {
        return x.min(y);
    }

    @Override
    protected int count(BigInteger x) {
        return x.bitCount();
    }
}
