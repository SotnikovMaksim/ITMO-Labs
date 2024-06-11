package generated.bad;

import java.util.Map;
import java.util.List;
import kotlin.Pair;
import kotlin.text.Regex;

public enum BadToken {
	PLUS("PLUS", "+", new Regex("+")),
	MULTIPLY("MULTIPLY", "*", new Regex("*")),
	LB("LB", "(", new Regex("(")),
	RB("RB", ")", new Regex(")")),
	EPS("EPS", "_EPS_", new Regex("_EPS_")),
    END("END","",new Regex(""));

    public String termName = "";
    public String termValue = "";
    public final Regex termRegexp;

    BadToken(String termName, String termValue, Regex termRegexp) {
        this.termName = termName;
        this.termValue = termValue;
        this.termRegexp = termRegexp;
    }

    public static final List<Pair<Regex, BadToken>> TOKEN_LIST = List.of(
			new Pair<>(new Regex("+"), PLUS),
			new Pair<>(new Regex("*"), MULTIPLY),
			new Pair<>(new Regex("("), LB),
			new Pair<>(new Regex(")"), RB),
			new Pair<>(new Regex("_EPS_"), EPS)
    );
}
