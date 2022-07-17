package expression.generic;

enum MyOperation {
    ADD("+", 2),
    SUBTRACT("-", 2),
    MULTIPLY("*", 2),
    DIVIDE("/", 2),
    VARIABLE(null, 0),
    MAX("max", 2),
    MIN("min", 2),
    DIGIT(null, 0),
    WHITESPACE(null, 0),
    END("\0", 0),
    LEFT_BRACKET("(", 2),
    RIGHT_BRACKET(")", 2),
    COUNT("count", 1);

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