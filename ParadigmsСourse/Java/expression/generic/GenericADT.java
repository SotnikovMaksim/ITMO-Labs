package expression.generic;

public abstract class GenericADT<T> {
    protected abstract T add(T x, T y);

    protected abstract T sub(T x, T y);

    protected abstract T mult(T x, T y);

    protected abstract T div(T x, T y);

    protected abstract T negate(T x);

    protected abstract T parse(int x);

    protected abstract T max(T x, T y);

    protected abstract T min(T x, T y);

    protected abstract int count(T x);
}
