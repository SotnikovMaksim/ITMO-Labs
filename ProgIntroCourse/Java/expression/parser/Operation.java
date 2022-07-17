package expression.parser;

enum Operation {
    MAX("max", 2),
    MIN("min", 2),
    ADD("+", 2),
    POW("**", 2),
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

    Operation(String operation, int count) {
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