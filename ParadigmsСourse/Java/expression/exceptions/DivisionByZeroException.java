package expression.exceptions;

public class DivisionByZeroException extends ParserException {

    public DivisionByZeroException(String message) {
        super(message);
    }
}
