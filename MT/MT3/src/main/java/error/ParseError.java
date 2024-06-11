package error;

public class ParseError extends RuntimeException {

    public ParseError(String message) {
        super(message);
    }

    public ParseError(String message, Throwable cause) {
        super(message, cause);
    }
}
