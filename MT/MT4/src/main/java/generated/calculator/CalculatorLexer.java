package generated.calculator;

import java.text.ParseException;
import kotlin.text.Regex;
import kotlin.text.MatchResult;

public class CalculatorLexer {

    private final String text;
    private final Regex tokenRegex = new Regex("\\+|-|\\*|/|\\(|\\)|[0-9]+[.][0-9]+|[0-9]+|_EPS_");

    private CalculatorToken tokens;
    private MatchResult matcher;
    private int pos;
    private CalculatorToken currToken;

    public CalculatorLexer(String text) throws ParseException {
        this.text = text;
        this.matcher = tokenRegex.find(text, 0);
        nextToken();
    }

    public CalculatorToken nextToken() throws ParseException {
        if (matcher == null) {
           currToken = CalculatorToken.END;
           return CalculatorToken.END;
        }
        while (pos < text.length() && Character.isWhitespace(text.charAt(pos))) pos++;
        if (matcher.getRange().getStart() != pos) throw new ParseException(text, pos);

        String currentValue = matcher.getValue();
        for (var checkedToken : CalculatorToken.TOKEN_LIST) {
            if (checkedToken.getFirst().matches(currentValue)) {
                String matchedText = matcher.getValue();
                pos += currentValue.length();
                matcher = matcher.next();
                currToken = Enum.valueOf(CalculatorToken.class, checkedToken.getSecond().termName);
                currToken.termValue = matchedText;
                return currToken;
            }
        }
        throw new ParseException(text, pos);
    }

    public CalculatorToken currentToken() {
        return currToken;
    }

    public int currentPos() {
        return pos;
    }
}