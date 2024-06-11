public enum Token {
    LPAREN("("),
    RPAREN(")"),
    CHAR_SEQ(null), // Set to null or an empty string initially
    ASTERISK("*"),
    PIPE("|"),
    END("$"),
    NOTHING("_");

    private String stringRepresentation; // Allow setting stringRepresentation dynamically

    Token(String stringRepresentation) {
        this.stringRepresentation = stringRepresentation;
    }

    public void setStringRepresentation(String stringRepresentation) {
        if (this == CHAR_SEQ) {
            this.stringRepresentation = stringRepresentation;
        } else {
            throw new IllegalStateException("Cannot set string representation for token: " + this);
        }
    }

    @Override
    public String toString() {
        return stringRepresentation;
    }
}
