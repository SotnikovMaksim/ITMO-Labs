package expression.generic;


import java.util.Objects;

public class Const<T> implements TripleGeneric<T> {
    public final T value;

    public Const(T value) {
        this.value = value;
    }

    @Override
    public T evaluate(T xValue, T yValue, T zValue) {
        return this.value;
    }

    public String toString() {
        return Objects.toString(value);
    }
}
