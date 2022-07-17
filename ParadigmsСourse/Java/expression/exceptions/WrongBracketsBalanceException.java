package expression.exceptions;

public class WrongBracketsBalanceException extends ParserException {
    public WrongBracketsBalanceException(String expression) {
        super("Brackets didn't place correctly in expression: " + expression);
    }
}
