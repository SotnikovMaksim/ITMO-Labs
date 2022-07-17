package expression.generic;


public class CheckedAdd<T> extends CheckedBinaryOperation<T> {
    public CheckedAdd(TripleGeneric<T> firstExp, TripleGeneric<T> secondExp, GenericADT<T> genericOperations) {
        super(firstExp, secondExp, "+", genericOperations);
    }

    @Override
    protected T eval(T firstEval, T secondEval, GenericADT<T> genericOperations) {
        return genericOperations.add(firstEval, secondEval);
    }
}

