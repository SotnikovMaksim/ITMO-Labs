package expression.generic;


public class CheckedMin<T> extends CheckedBinaryOperation<T> {
    public CheckedMin(TripleGeneric<T> firstExp, TripleGeneric<T> secondExp, GenericADT<T> genericOperations) {
        super(firstExp, secondExp, "max", genericOperations);
    }

    @Override
    public T eval(T x, T y, GenericADT<T> genericOperations) {
        return genericOperations.min(x, y);
    }
}
