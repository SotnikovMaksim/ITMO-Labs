package generated.bad;

import java.text.ParseException;
import kotlin.text.Regex;
import kotlin.text.MatchResult;

public class BadLexer {

    private final String text;
    private final Regex tokenRegex = new Regex("+|*|(|)|_EPS_");

    private BadToken tokens;
    private MatchResult matcher;
    private int pos;
    private BadToken currToken;

    public BadLexer(String text) throws ParseException {
        this.text = text;
        this.matcher = tokenRegex.find(text, 0);
        nextToken();
    }

    public BadToken nextToken() throws ParseException {
        if (matcher == null) {
           currToken = BadToken.END;
           return BadToken.END;
        }
        while (pos < text.length() && Character.isWhitespace(text.charAt(pos))) pos++;
        if (matcher.getRange().getStart() != pos) throw new ParseException(text, pos);

        String currentValue = matcher.getValue();
        for (var checkedToken : BadToken.TOKEN_LIST) {
            if (checkedToken.getFirst().matches(currentValue)) {
                String matchedText = matcher.getValue();
                pos += currentValue.length();
                matcher = matcher.next();
                currToken = Enum.valueOf(BadToken.class, checkedToken.getSecond().termName);
                currToken.termValue = matchedText;
                return currToken;
            }
        }
        throw new ParseException(text, pos);
    }

    public BadToken currentToken() {
        return currToken;
    }

    public int currentPos() {
        return pos;
    }
}