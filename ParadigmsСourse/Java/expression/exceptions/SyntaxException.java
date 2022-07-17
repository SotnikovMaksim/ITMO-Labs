package expression.exceptions;

public class SyntaxException extends ParserException {
    public SyntaxException(String message) {
        super("syntax excetion: " + message);
    }
}
