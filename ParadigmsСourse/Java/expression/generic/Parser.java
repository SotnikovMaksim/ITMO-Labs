package expression.generic;


public interface Parser<T> {
    TripleGeneric<T> parse(String expression);
}
