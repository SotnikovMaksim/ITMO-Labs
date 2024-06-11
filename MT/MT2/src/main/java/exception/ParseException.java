package exception;

public class ParseException extends RuntimeException {
    public ParseException(String message, int curPos) {
        super("Error occurred due parsing at pos: %d\nError: %s"
                .formatted(curPos, message));
    }
}
