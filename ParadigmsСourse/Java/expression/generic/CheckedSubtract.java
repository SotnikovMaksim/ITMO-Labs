package expression.generic;


public class CheckedSubtract<T> extends CheckedBinaryOperation<T> {
    public CheckedSubtract(TripleGeneric<T> firstExp, TripleGeneric<T> secondExp, GenericADT<T> genericOperations) {
        super(firstExp, secondExp, "-", genericOperations);
    }

    @Override
    protected T eval(T firstEval, T secondEval, GenericADT<T> genericOperations) {
        return genericOperations.sub(firstEval, secondEval);
    }
}
