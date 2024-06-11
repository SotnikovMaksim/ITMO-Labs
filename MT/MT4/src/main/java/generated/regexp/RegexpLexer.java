package generated.regexp;

import java.text.ParseException;
import kotlin.text.Regex;
import kotlin.text.MatchResult;

public class RegexpLexer {

    private final String text;
    private final Regex tokenRegex = new Regex("\\||\\*|\\(|\\)|[a-z]|_EPS_");

    private RegexpToken tokens;
    private MatchResult matcher;
    private int pos;
    private RegexpToken currToken;

    public RegexpLexer(String text) throws ParseException {
        this.text = text;
        this.matcher = tokenRegex.find(text, 0);
        nextToken();
    }

    public RegexpToken nextToken() throws ParseException {
        if (matcher == null) {
           currToken = RegexpToken.END;
           return RegexpToken.END;
        }
        while (pos < text.length() && Character.isWhitespace(text.charAt(pos))) pos++;
        if (matcher.getRange().getStart() != pos) throw new ParseException(text, pos);

        String currentValue = matcher.getValue();
        for (var checkedToken : RegexpToken.TOKEN_LIST) {
            if (checkedToken.getFirst().matches(currentValue)) {
                String matchedText = matcher.getValue();
                pos += currentValue.length();
                matcher = matcher.next();
                currToken = Enum.valueOf(RegexpToken.class, checkedToken.getSecond().termName);
                currToken.termValue = matchedText;
                return currToken;
            }
        }
        throw new ParseException(text, pos);
    }

    public RegexpToken currentToken() {
        return currToken;
    }

    public int currentPos() {
        return pos;
    }
}