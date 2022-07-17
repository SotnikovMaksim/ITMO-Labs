package expression.generic;


public class CheckedMax<T> extends CheckedBinaryOperation<T> {
    public CheckedMax(TripleGeneric<T> firstExp, TripleGeneric<T> secondExp, GenericADT<T> genericOperations) {
        super(firstExp, secondExp, "max", genericOperations);
    }

    @Override
    public T eval(T x, T y, GenericADT<T> genericOperations) {
        return genericOperations.max(x, y);
    }
}