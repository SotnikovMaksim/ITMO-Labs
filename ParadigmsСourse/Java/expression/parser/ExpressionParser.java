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
        if (next() == MyOperation.WHITESPACE) {
            skipWhitespace();
        }
        return expression();
    }

    private TripleExpression expression() {
        TripleExpression result = nextBase();
        MyOperation operation = next();
        while (operation != MyOperation.END) {
            if (operation == MyOperation.MAX) {
                skipWhitespace();
                result = operation(MyOperation.MAX.getOperation(), result, nextBase());
            } else if (operation == MyOperation.MIN) {
                skipWhitespace();
                result = operation(MyOperation.MIN.getOperation(), result, nextBase());
            } else if (operation.getCount() == 2) {
                break;
            }
            operation = next();
        }
        return result;
    }

    private TripleExpression nextBase() {
        TripleExpression res = nextExponent();
        MyOperation operation = next();
        while (operation != MyOperation.END) {
            if (operation == MyOperation.ADD) {
                skipWhitespace();
                res = operation(MyOperation.ADD.getOperation(), res, nextExponent());
            } else if (operation == MyOperation.SUBTRACT) {
                skipWhitespace();
                res = operation(MyOperation.SUBTRACT.getOperation(), res, nextExponent());
            } else if (operation.getCount() == 2) {
                break;
            }
            operation = next();
        }
        return res;
    }

    private TripleExpression nextExponent() {
        TripleExpression res = nextExpression();
        MyOperation current = next();
        while (current != MyOperation.END) {
            if (current == MyOperation.MULTIPLY) {
                skipWhitespace();
                res = operation(MyOperation.MULTIPLY.getOperation(), res, nextExpression());
            } else if (current == MyOperation.DIVIDE) {
                skipWhitespace();
                res = operation(MyOperation.DIVIDE.getOperation(), res, nextExpression());
            } else if (current.getCount() == 2) {
                break;
            }
            current = next();
        }
        return res;
    }

    private TripleExpression nextExpression() {
        MyOperation current = next();
        if (current == MyOperation.DIGIT) {
            return new Const(nextConst());
        }
        if (current == MyOperation.LEFT_BRACKET) {
            skipWhitespace();
            TripleExpression content = expression();
            skipWhitespace();
            return content;
        }
        if (current == MyOperation.VARIABLE) {
            TripleExpression var = new Variable(String.valueOf(expression.charAt(pos)));
            skipWhitespace();
            return var;
        }
        if (current == MyOperation.SUBTRACT) {
            if (nextOperation(pos + next().getLength()) == MyOperation.DIGIT) {
                return new Const(nextConst());
            }
            skipWhitespace();
            return new Negate(nextExpression());
        }
        if (current == MyOperation.LEFT_ZEROES || current == MyOperation.RIGHT_ZEROES) {
            skipWhitespace();
            TripleExpression content = nextExpression();
            return new Zeroes(content, current.getOperation());
        }
        throw error("Wrong operation exception");
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

    private MyOperation next() {
        return nextOperation(pos);
    }

    private void skipWhitespace() {
        pos += next().getLength();
        while (next() == MyOperation.WHITESPACE) {
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
        if (next() == MyOperation.SUBTRACT) {
            number.append(expression.charAt(i++));
            skipWhitespace();
        }
        while (next() == MyOperation.DIGIT) {
            number.append(expression.charAt(i++));
            skipWhitespace();
        }
        return Integer.parseInt(number.toString());
    }
}
