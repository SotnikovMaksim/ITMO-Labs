package expression.generic;


public class CheckedNegate<T> implements TripleGeneric<T> {
    private final TripleGeneric<T> expression;
    private final GenericADT<T> genericOperations;

    public CheckedNegate(TripleGeneric<T> expression, GenericADT<T> genericOperations) {
        this.expression = expression;
        this.genericOperations = genericOperations;
    }

    public String toString() {
        return "-(" + expression.toString() + ")";
    }

    @Override
    public T evaluate(T x, T y, T z) {
        T result = expression.evaluate(x, y, z);
        return genericOperations.negate(result);
    }
}
