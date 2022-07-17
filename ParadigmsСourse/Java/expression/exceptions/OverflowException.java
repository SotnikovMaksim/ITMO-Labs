package expression.exceptions;

public class OverflowException extends ParserException {
    public OverflowException(String message) {
        super("Overflow " + message);
    }
}