package expression.exceptions;

import expression.Const;
import expression.TripleExpression;
import expression.Variable;

public class ExpressionParser implements Parser {
    private char[] expression;
    private String stringExpression;
    private int pos;
    private int length;

    public TripleExpression parse(String expression) throws ParserException {
        this.stringExpression = expression;
        this.expression = expression.toCharArray();
        length = expression.length();
        pos = 0;
        bracketBalance();
        if (nextOperation(pos) == MyOperation.WHITESPACE) {
            skipWhitespace();
        }
        return expression();
    }

    private TripleExpression expression() {
        TripleExpression result = nextBase();
        MyOperation operation = nextOperation(pos);
        while (operation != MyOperation.END) {
            if (operation == MyOperation.MAX || operation == MyOperation.MIN) {
                if (Character.isWhitespace(expression[pos - 1]) || expression[pos - 1] == ')') {
                    skipWhitespace();
                    result = operation(operation.getOperation(), result, nextBase());
                } else {
                    throw new UnexpectedSymbolException(pos + operation.getLength() + 1, stringExpression);
                }
            } else if (operation.getCount() == 2) {
                break;
            } else {
                throw new UnexpectedSymbolException(pos, stringExpression);
            }
            operation = nextOperation(pos);
        }
        return result;
    }

    private TripleExpression nextBase() {
        TripleExpression res = nextExponent();
        MyOperation operation = nextOperation(pos);
        while (operation != MyOperation.END) {
            if (operation == MyOperation.ADD || operation == MyOperation.SUBTRACT) {
                skipWhitespace();
                res = operation(operation.getOperation(), res, nextExponent());
            } else if (operation.getCount() == 2) {
                break;
            }
            operation = nextOperation(pos);
        }
        return res;
    }

    private TripleExpression nextExponent() {
        TripleExpression res = nextExpression();
        MyOperation current = nextOperation(pos);
        while (current != MyOperation.END) {
            if (current == MyOperation.MULTIPLY || current == MyOperation.DIVIDE) {
                skipWhitespace();
                res = operation(current.getOperation(), res, nextExpression());
            } else if (current.getCount() == 2) {
                break;
            }
            current = nextOperation(pos);
        }
        return res;
    }

    private TripleExpression nextExpression() {
        MyOperation current = nextOperation(pos);
        if (current == MyOperation.DIGIT) {
            return new Const(nextConst());
        } else if (current == MyOperation.LEFT_BRACKET) {
            skipWhitespace();
            TripleExpression content = expression();
            skipWhitespace();
            return content;
        } else if (current == MyOperation.VARIABLE) {
            TripleExpression var = new Variable(String.valueOf(expression[pos]));
            skipWhitespace();
            return var;
        } else if (current == MyOperation.SUBTRACT) {
            if (nextOperation(pos + 1) == MyOperation.DIGIT) {
                return new Const(nextConst());
            }
            skipWhitespace();
            return new CheckedNegate(nextExpression());
        } else if (current == MyOperation.LEFT_ZEROES || current == MyOperation.RIGHT_ZEROES) {
            if (expression[pos + 2] == '(' || expression[pos + 2] == ' ') {
                skipWhitespace();
                return new CheckedZeroes(nextExpression(), current.getOperation());
            } else {
                throw new UnexpectedSymbolException(pos + current.getLength() + 1, stringExpression);
            }
        }
        throw new NotValidSymbolException(stringExpression, pos);
    }

    private MyOperation nextOperation(int pos) {
        if (pos >= length)
            return MyOperation.END;
        if (expression[pos] == '+') {
            return MyOperation.ADD;
        } else if (expression[pos] == '-') {
            return MyOperation.SUBTRACT;
        } else if (expression[pos] == '*') {
            return MyOperation.MULTIPLY;
        } else if (expression[pos] == '/') {
            return MyOperation.DIVIDE;
        } else if (expression[pos] == '(') {
            return MyOperation.LEFT_BRACKET;
        } else if (expression[pos] == ')') {
            return MyOperation.RIGHT_BRACKET;
        } else if (Character.isDigit(expression[pos])) {
            return MyOperation.DIGIT;
        } else if (Character.isWhitespace(expression[pos])) {
            return MyOperation.WHITESPACE;
        } else if (expression[pos] == 'x' || expression[pos] == 'y' || expression[pos] == 'z') {
            return MyOperation.VARIABLE;
        } else if ((pos + 2 < length) && (expression[pos + 1] == '0')) {
            if (expression[pos] == 'l') {
                return MyOperation.LEFT_ZEROES;
            } else if (expression[pos] == 't') {
                return MyOperation.RIGHT_ZEROES;
            }
        } else if ((pos + 2 < length) && (expression[pos] == 'm')) {
            if (expression[pos + 1] == 'a' && expression[pos + 2] == 'x') {
                return MyOperation.MAX;
            } else if (expression[pos + 1] == 'i' && expression[pos + 2] == 'n') {
                return MyOperation.MIN;
            }
        }
        throw new NotValidSymbolException(stringExpression, pos);
    }

    private TripleExpression operation(String operator, TripleExpression firstExp, TripleExpression secondExp) throws ParserException {
        switch (operator) {
            case ("+"):
                return new CheckedAdd(firstExp, secondExp);
            case ("-"):
                return new CheckedSubtract(firstExp, secondExp);
            case ("*"):
                return new CheckedMultiply(firstExp, secondExp);
            case ("/"):
                return new CheckedDivide(firstExp, secondExp);
            case ("max"):
                return new CheckedMax(firstExp, secondExp);
            case ("min"):
                return new CheckedMin(firstExp, secondExp);
            default:
                throw new NotValidSymbolException(stringExpression, pos);
        }
    }

    private void skipWhitespace() {
        pos += nextOperation(pos).getLength();
        while (nextOperation(pos) == MyOperation.WHITESPACE) {
            pos++;
        }
    }

    private int nextConst() {
        int i = pos;
        StringBuilder number = new StringBuilder();
        if (nextOperation(pos) == MyOperation.SUBTRACT) {
            number.append(expression[i++]);
            skipWhitespace();
        }
        while (nextOperation(pos) == MyOperation.DIGIT) {
            number.append(expression[i++]);
            skipWhitespace();
        }
        return Integer.parseInt(number.toString());
    }

    private void bracketBalance() throws ParserException {
        int brackets = 0;
        for (char c : expression) {
            if (c == '(') {
                brackets++;
            }
            if (c == ')') {
                brackets--;
            }
            if (brackets < 0) {
                throw new WrongBracketsBalanceException(stringExpression);
            }
        }
        if (brackets != 0) {
            throw new WrongBracketsBalanceException(stringExpression);
        }
    }

    enum MyOperation {
        MAX("max", 2),
        MIN("min", 2),
        ADD("+", 2),
        SUBTRACT("-", 2),
        MULTIPLY("*", 2),
        DIVIDE("/", 2),
        VARIABLE(null, 0),
        DIGIT(null, 0),
        WHITESPACE(null, 0),
        END("\0", 0),
        LEFT_BRACKET("(", 2),
        RIGHT_BRACKET(")", 2),
        LEFT_ZEROES("l0", 1),
        RIGHT_ZEROES("t0", 1);

        private final String operation;
        private final int count;

        MyOperation(String operation, int count) {
            this.operation = operation;
            this.count = count;
        }

        public String getOperation() {
            return operation;
        }

        public int getCount() {
            return count;
        }

        public int getLength() {
            return operation != null ? getOperation().length() : 1;
        }
    }
}
