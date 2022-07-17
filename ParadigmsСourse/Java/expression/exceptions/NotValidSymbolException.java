package expression.exceptions;

public class NotValidSymbolException extends SyntaxException {
    public NotValidSymbolException(String expression, int pos) {
        super(expression + '\n' + " ".repeat(pos) + "^");
    }
}
