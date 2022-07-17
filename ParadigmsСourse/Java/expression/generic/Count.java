package expression.generic;

public class Count<T> implements TripleGeneric<T> {
    private final TripleGeneric<T> expression;
    private final GenericADT<T> genericOperations;

    public Count(TripleGeneric<T> expression, GenericADT<T> genericOperations) {
        this.expression = expression;
        this.genericOperations = genericOperations;
    }

    public String toString() {
        return "count (" + expression.toString() + ")";
    }

    public T evaluate(T x, T y, T z) {
        T result = expression.evaluate(x, y, z);
        return genericOperations.parse(genericOperations.count(result));
    }
}
