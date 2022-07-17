package expression.parser;

import expression.*;

public class ExpressionParser implements Parser {
    private String expression;
    private int pos;
    private int length;

    public ExpressionParser() {
    }

    public TripleExpression parse(String expression) {
        this.expression = expression;
        length = expression.length();
        pos = 0;
        if (next() == Operation.WHITESPACE) {
            skipWhitespace();
        }
        return expression();
    }

    private TripleExpression expression() {
        TripleExpression result = nextBase();
        Operation operation = next();
        while (operation != Operation.END) {
            if (operation == Operation.MAX) {
                skipWhitespace();
                result = operation(Operation.MAX.getOperation(), result, nextBase());
            } else if (operation == Operation.MIN) {
                skipWhitespace();
                result = operation(Operation.MIN.getOperation(), result, nextBase());
            } else if (operation.getCount() == 2) {
                break;
            }
            operation = next();
        }
        return result;
    }

    private TripleExpression nextBase() {
        TripleExpression res = nextExponent();
        Operation operation = next();
        while (operation != Operation.END) {
            if (operation == Operation.ADD) {
                skipWhitespace();
                res = operation(Operation.ADD.getOperation(), res, nextExponent());
            } else if (operation == Operation.SUBTRACT) {
                skipWhitespace();
                res = operation(Operation.SUBTRACT.getOperation(), res, nextExponent());
            } else if (operation.getCount() == 2) {
                break;
            }
            operation = next();
        }
        return res;
    }


    private TripleExpression nextExponent() {
        TripleExpression res = nextExpression();
        Operation current = next();
        while (current != Operation.END) {
            if (current == Operation.MULTIPLY) {
                skipWhitespace();
                res = operation(Operation.MULTIPLY.getOperation(), res, nextExpression());
            } else if (current == Operation.DIVIDE) {
                skipWhitespace();
                res = operation(Operation.DIVIDE.getOperation(), res, nextExpression());
            } else if (current.getCount() == 2) {
                break;
            }
            current = next();
        }
        return res;
    }

    private TripleExpression nextExpression() {
        Operation current = next();
        if (current == Operation.DIGIT) {
            return new Const(nextConst());
        }
        if (current == Operation.LEFT_BRACKET) {
            skipWhitespace();
            TripleExpression content = expression();
            skipWhitespace();
            return content;
        }
        if (current == Operation.VARIABLE) {
            TripleExpression var = new Variable(String.valueOf(expression.charAt(pos)));
            skipWhitespace();
            return var;
        }
        if (current == Operation.SUBTRACT) {
            if (nextOperation(pos + next().getLength()) == Operation.DIGIT) {
                return new Const(nextConst());
            }
            skipWhitespace();
            return new Negate(nextExpression());
        }
        if (current == Operation.LEFT_ZEROES || current == Operation.RIGHT_ZEROES) {
            skipWhitespace();
            TripleExpression content = nextExpression();
            return new Zeroes(content, current.getOperation());
        }
        throw error("Wrong operation exception");
    }

    private Operation nextOperation(int pos) {
        if (pos >= length)
            return Operation.END;
        for (Operation operation : Operation.values()) {
            if (length - pos >= operation.getLength() && expression.substring(pos, pos + operation.getLength()).equals(operation.getOperation())) {
                return operation;
            }
        }
        if (Character.isDigit(expression.charAt(pos))) {
            return Operation.DIGIT;
        }
        if (Character.isWhitespace(expression.charAt(pos))) {
            return Operation.WHITESPACE;
        }
        if (expression.charAt(pos) == 'x' || expression.charAt(pos) == 'y' || expression.charAt(pos) == 'z') {
            return Operation.VARIABLE;
        }
        throw error("Wrong operation exception");
    }

    private TripleExpression operation(String operator, TripleExpression firstExp, TripleExpression secondExp) {
        if (operator.equals("+")) {
            return new Add((Common) firstExp, (Common) secondExp);
        }
        if (operator.equals("-")) {
            return new Subtract((Common) firstExp, (Common) secondExp);
        }
        if (operator.equals("*")) {
            return new Multiply((Common) firstExp, (Common) secondExp);
        }
        if (operator.equals("/")) {
            return new Divide((Common) firstExp, (Common) secondExp);
        }
        if (operator.equals("max")) {
            return new Min((Common) firstExp, (Common) secondExp);
        }
        if (operator.equals("min")) {
            return new Max((Common) firstExp, (Common) secondExp);
        }
        throw error("Unknown operation exception");
    }

    private Operation next() {
        return nextOperation(pos);
    }

    private void skipWhitespace() {
        pos += next().getLength();
        while (next() == Operation.WHITESPACE) {
            pos++;
        }
    }

    private IllegalArgumentException error(String message) throws IllegalArgumentException {
        return new IllegalArgumentException(String.format(
                message + ": %d, %s.", pos, expression.charAt(pos)
        ));
    }

    private int nextConst() {
        int i = pos;
        StringBuilder number = new StringBuilder();
        if (next() == Operation.SUBTRACT) {
            number.append(expression.charAt(i++));
            skipWhitespace();
        }
        while (next() == Operation.DIGIT) {
            number.append(expression.charAt(i++));
            skipWhitespace();
        }
        return Integer.parseInt(number.toString());
    }
}
