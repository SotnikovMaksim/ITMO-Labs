import exception.ParseException;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

public class Parser {

    private LexicalAnalyzer lex;

    public Parser() {
    }

    private Tree E() {
        if (lex.curToken() == Token.END ||
                lex.curToken() == Token.NOTHING
        ) {
            return new Tree("E", new Tree(lex.curToken().toString()));
        }
        return new Tree(
                "E",
                P(),
                E()
        );
    }

    private Tree P() {
        return new Tree(
                "P",
                A(),
                PPrime()
        );
    }

    private Tree PPrime() {
        if (lex.curToken() == Token.PIPE) {
            final List<Tree> children = new ArrayList<>();

            children.add(new Tree(Token.PIPE.toString()));

            while (lex.curToken() == Token.PIPE) {
                lex.nextToken();
                children.add(new Tree(
                                "A",
                                A()
                        )
                );
            }

            return new Tree("P'", children.toArray(new Tree[0]));
        } else {
            return new Tree("P'", new Tree(Token.NOTHING.toString()));
        }
    }

    private Tree A() {
        return new Tree(
                "A",
                S(),
                APrime()
        );
    }

    private Tree APrime() {
        if (lex.curToken() == Token.ASTERISK) {
            final List<Tree> children = new ArrayList<>();

            while (lex.curToken() == Token.ASTERISK) {
                lex.nextToken();
                children.add(new Tree("A'", new Tree(
                                Token.ASTERISK.toString()
                        ))
                );
            }

            return new Tree("A'", children.toArray(new Tree[0]));
        } else {
            return new Tree("A'", new Tree(Token.NOTHING.toString()));
        }
    }

//    private Tree APrime() {
//        if (lex.curToken() == Token.ASTERISK) {
//            lex.nextToken();
//            return new Tree(Token.ASTERISK.toString());
//        }
//        return new Tree(Token.NOTHING.toString());
//    }

    private Tree S() {
        Token curToken = lex.curToken();

        switch (curToken) {
            case LPAREN -> {
                lex.nextToken();
                final Tree subExpr = E();
                lex.nextToken();

                if (lex.curToken() != Token.RPAREN) {
                    throwParseException(Token.RPAREN, lex.curToken(), lex.curPos());
                }

                lex.nextToken();
                return new Tree("S", new Tree(
                        Token.LPAREN.toString(),
                        subExpr,
                        new Tree(Token.RPAREN.toString())
                ));
            }
            case CHAR_SEQ -> {
                lex.nextToken();
                return new Tree("S", new Tree(curToken.toString()));
            }
            default -> throw new ParseException("Unexpected token: " + curToken, lex.curPos());
        }
    }

    private void throwParseException(Token expected, Token got, int pos) {
        throw new ParseException("Expected %s, but got %s"
                .formatted(expected, got), pos);
    }

    public Tree parse(InputStream is) throws ParseException {
        lex = new LexicalAnalyzer(is);
        lex.nextToken();
        final Tree tree = E();

//      Checking end of expression
        lex.nextToken();
        if (lex.curToken() != Token.NOTHING &&
                lex.curToken() != Token.END
        ) {
            throw new ParseException("Expected end of expression but found: " + lex.curToken(), lex.curPos());
        }

        return tree;
    }
}
