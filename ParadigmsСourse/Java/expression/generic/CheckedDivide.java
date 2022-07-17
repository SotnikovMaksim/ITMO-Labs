package expression.generic;


public class CheckedDivide<T> extends CheckedBinaryOperation<T> {
    public CheckedDivide(TripleGeneric<T> firstExp, TripleGeneric<T> secondExp, GenericADT<T> genericOperations) {
        super(firstExp, secondExp, "/", genericOperations);
    }

    @Override
    protected T eval(T firstEval, T secondEval, GenericADT<T> genericOperations) {
        return genericOperations.div(firstEval, secondEval);
    }
}
