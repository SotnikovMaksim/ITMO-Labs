package expression.generic;


public class Variable<T> implements TripleGeneric<T> {
    public final String name;


    public Variable(String name) {
        this.name = name;
    }

    public T evaluate(T xValue, T yValue, T zValue) {
        switch (name) {
            case "x":
                return xValue;
            case "y":
                return yValue;
            case "z":
                return zValue;
            default:
                throw new IllegalArgumentException();
        }
    }

    public String toString() {
        return name;
    }
}
