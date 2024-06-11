import exception.ParseException;

import java.io.IOException;
import java.io.InputStream;

public class LexicalAnalyzer {

    private final InputStream is;

    private Token curToken;

    private int curChar;
    private int curPos;

    public LexicalAnalyzer(final InputStream is) {
        this.is = is;
        curPos = 0;
        nextChar();
    }

    private void nextChar() {
        if (curToken == Token.NOTHING) {
            return;
        }

        curPos++;
        try {
            curChar = is.read();
        } catch (IOException e) {
            throw new ParseException(e.getMessage(), curPos);
        }
    }

    private boolean isBlank(int c) {
        return Character.isWhitespace(c);
    }

    public void nextToken() {
        while (isBlank(curChar)) {
            nextChar();
        }

        if (curChar == '(') {
            curToken = Token.LPAREN;
            nextChar();
        } else if (curChar == ')') {
            if (curToken == Token.NOTHING) {
                curToken = Token.RPAREN;
            } else {
                curToken = Token.NOTHING;
            }
            nextChar();
        } else if (curChar == '*') {
            curToken = Token.ASTERISK;
            nextChar();
        } else if (curChar == '|') {
            curToken = Token.PIPE;
            nextChar();
        } else if (Character.isLetter(curChar)) {
            String charSeq = parseString();

            curToken = Token.CHAR_SEQ;
            curToken.setStringRepresentation(charSeq);
        } else if (curChar == -1) {
            curToken = (curToken == Token.NOTHING) ? Token.END : Token.NOTHING;
        } else {
            throw new ParseException("Unexpected character: " + curChar, curPos);
        }
    }

    private String parseString() {
        StringBuilder sb = new StringBuilder();

        while (Character.isLetter(curChar)) {
            sb.append((char) curChar);
            nextChar();
        }

        return sb.toString();
    }

    public Token curToken () {
        return curToken;
    }

    public int curPos() {
        return curPos;
    }
}
