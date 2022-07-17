package expression.generic;


public abstract class CheckedBinaryOperation<T> implements TripleGeneric<T> {
    public final TripleGeneric<T> firstExp;
    public final TripleGeneric<T> secondExp;
    public final String operator;
    private final GenericADT<T> genericOperations;

    protected abstract T eval(T firstValue, T secondValue, GenericADT<T> genericOperations);

    public CheckedBinaryOperation(TripleGeneric<T> firstExp, TripleGeneric<T> secondExp, String operator, GenericADT<T> genericOperations) {
        this.firstExp = firstExp;
        this.secondExp = secondExp;
        this.operator = " " + operator + " ";
        this.genericOperations = genericOperations;
    }

    public String toString() {
        return "(" + firstExp.toString() + operator + secondExp.toString() + ")";
    }

    public T evaluate(T x, T y, T z) {
        return eval(firstExp.evaluate(x, y, z), secondExp.evaluate(x, y, z), genericOperations);
    }
}
