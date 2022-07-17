package expression.generic;

import expression.exceptions.*;

public class ExpressionParser<T> implements Parser<T> {
    private final GenericADT<T> genericOperations;
    private String expression;
    private int pos;
    private int length;

    public ExpressionParser(GenericADT<T> genericOperations) {
        this.genericOperations = genericOperations;
    }

    public TripleGeneric<T> parse(String expression) throws ParserException {
        this.expression = expression;
        length = expression.length();
        pos = 0;
        bracketBalance();
        if (nextOperation(pos) == MyOperation.WHITESPACE) {
            skipWhitespace();
        }
        return expression();
    }

    private TripleGeneric<T> expression() {
        TripleGeneric<T> result = nextBase();
        MyOperation operation = nextOperation(pos);
        while (operation != MyOperation.END) {
            if (operation == MyOperation.MAX || operation == MyOperation.MIN) {
                if (Character.isWhitespace(expression.charAt(pos - 1)) || expression.charAt(pos - 1) == ')') {
                    skipWhitespace();
                    result = binaryOperation(operation.getOperation(), result, nextBase());
                } else {
                    throw new UnexpectedSymbolException(pos + operation.getLength() + 1, expression);
                }
            } else if (operation.getCount() == 2) {
                break;
            } else {
                throw new UnexpectedSymbolException(pos, expression);
            }
            operation = nextOperation(pos);
        }
        return result;
    }

    private TripleGeneric<T> nextBase() {
        TripleGeneric<T> res = nextExponent();
        MyOperation operation = nextOperation(pos);
        while (operation != MyOperation.END) {
            if (operation == MyOperation.ADD) {
                skipWhitespace();
                res = binaryOperation(MyOperation.ADD.getOperation(), res, nextExponent());
            } else if (operation == MyOperation.SUBTRACT) {
                skipWhitespace();
                res = binaryOperation(MyOperation.SUBTRACT.getOperation(), res, nextExponent());
            } else if (operation.getCount() == 2) {
                break;
            }
            operation = nextOperation(pos);
        }
        return res;
    }


    private TripleGeneric<T> nextExponent() {
        TripleGeneric<T> res = nextExpression();
        MyOperation current = nextOperation(pos);
        while (current != MyOperation.END) {
            if (current == MyOperation.MULTIPLY) {
                skipWhitespace();
                res = binaryOperation(MyOperation.MULTIPLY.getOperation(), res, nextExpression());
            } else if (current == MyOperation.DIVIDE) {
                skipWhitespace();
                res = binaryOperation(MyOperation.DIVIDE.getOperation(), res, nextExpression());
            } else if (current.getCount() == 2) {
                break;
            }
            current = nextOperation(pos);
        }
        return res;
    }

    private TripleGeneric<T> nextExpression() {
        MyOperation current = nextOperation(pos);
        if (current == MyOperation.DIGIT) {
            return new Const<>(nextConst());
        }
        if (current == MyOperation.LEFT_BRACKET) {
            skipWhitespace();
            TripleGeneric<T> content = expression();
            skipWhitespace();
            return content;
        }
        if (current == MyOperation.VARIABLE) {
            TripleGeneric<T> var = new Variable<>(String.valueOf(expression.charAt(pos)));
            skipWhitespace();
            return var;
        }
        if (current == MyOperation.SUBTRACT) {
            if (nextOperation(pos + nextOperation(pos).getLength()) == MyOperation.DIGIT) {
                return new Const<>(nextConst());
            }
            skipWhitespace();
            return new CheckedNegate<>(nextExpression(), genericOperations);
        }
        if (current == MyOperation.COUNT) {
            skipWhitespace();
            TripleGeneric<T> c = new Count<>(nextExpression(), genericOperations);
            return c;
        }
        throw new NotValidSymbolException(expression, pos);
    }

    private MyOperation nextOperation(int pos) {
        if (pos >= length)
            return MyOperation.END;
        for (MyOperation operation : MyOperation.values()) {
            if (length - pos >= operation.getLength() && expression.substring(pos, pos + operation.getLength()).equals(operation.getOperation())) {
                return operation;
            }
        }
        if (Character.isDigit(expression.charAt(pos))) {
            return MyOperation.DIGIT;
        }
        if (Character.isWhitespace(expression.charAt(pos))) {
            return MyOperation.WHITESPACE;
        }
        if (expression.charAt(pos) == 'x' || expression.charAt(pos) == 'y' || expression.charAt(pos) == 'z') {
            return MyOperation.VARIABLE;
        }
        throw new NotValidSymbolException(expression, pos);
    }

    private TripleGeneric<T> binaryOperation(String operator, TripleGeneric<T> firstExp, TripleGeneric<T> secondExp) throws ParserException {
        if (operator.equals("+")) {
            return new CheckedAdd<>(firstExp, secondExp, genericOperations);
        }
        if (operator.equals("-")) {
            return new CheckedSubtract<>(firstExp, secondExp, genericOperations);
        }
        if (operator.equals("*")) {
            return new CheckedMultiply<>(firstExp, secondExp, genericOperations);
        }
        if (operator.equals("/")) {
            return new CheckedDivide<>(firstExp, secondExp, genericOperations);
        }
        if (operator.equals("max")) {
            return new CheckedMax<>(firstExp, secondExp, genericOperations);
        }
        if (operator.equals("min")) {
            return new CheckedMin<>(firstExp, secondExp, genericOperations);
        }
        throw new NotValidSymbolException(expression, pos);
    }

    private void skipWhitespace() {
        pos += nextOperation(pos).getLength();
        while (nextOperation(pos) == MyOperation.WHITESPACE) {
            pos++;
        }
    }

    private T nextConst() {
        int i = pos;
        StringBuilder number = new StringBuilder();
        if (nextOperation(pos) == MyOperation.SUBTRACT) {
            number.append(expression.charAt(i++));
            skipWhitespace();
        }
        while (nextOperation(pos) == MyOperation.DIGIT) {
            number.append(expression.charAt(i++));
            skipWhitespace();
        }
        return genericOperations.parse(Integer.parseInt(number.toString()));
    }

    private void bracketBalance() throws ParserException {
        int brackets = 0;
        for (int i = 0; i < expression.length(); i++) {
            if (expression.charAt(i) == '(') {
                brackets++;
            }
            if (expression.charAt(i) == ')') {
                brackets--;
            }
            if (brackets < 0) {
                throw new WrongBracketsBalanceException(expression);
            }
        }
        if (brackets != 0) {
            throw new WrongBracketsBalanceException(expression);
        }
    }
}
