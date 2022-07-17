package expression.generic;


public class CheckedMultiply<T> extends CheckedBinaryOperation<T> {
    public CheckedMultiply(TripleGeneric<T> firstExp, TripleGeneric<T> secondExp, GenericADT<T> genericOperations) {
        super(firstExp, secondExp, "*", genericOperations);
    }

    @Override
    protected T eval(T firstEval, T secondEval, GenericADT<T> genericOperations) {
        return genericOperations.mult(firstEval, secondEval);
    }
}