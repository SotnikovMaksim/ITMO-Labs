package expression.exceptions;

public class UnexpectedSymbolException extends SyntaxException {
    public UnexpectedSymbolException(int pos, String expression) {
        super(expression + '\n' + " ".repeat(pos) + "^");
    }
}
