package generated.calculator;

import java.util.Map;
import java.util.List;
import kotlin.Pair;
import kotlin.text.Regex;

public enum CalculatorToken {
	PLUS("PLUS", "\\+", new Regex("\\+")),
	MINUS("MINUS", "-", new Regex("-")),
	MULT("MULT", "\\*", new Regex("\\*")),
	DIV("DIV", "/", new Regex("/")),
	LP("LP", "\\(", new Regex("\\(")),
	RP("RP", "\\)", new Regex("\\)")),
	FLOAT("FLOAT", "[0-9]+[.][0-9]+", new Regex("[0-9]+[.][0-9]+")),
	INT("INT", "[0-9]+", new Regex("[0-9]+")),
	EPS("EPS", "_EPS_", new Regex("_EPS_")),
    END("END","",new Regex(""));

    public String termName = "";
    public String termValue = "";
    public final Regex termRegexp;

    CalculatorToken(String termName, String termValue, Regex termRegexp) {
        this.termName = termName;
        this.termValue = termValue;
        this.termRegexp = termRegexp;
    }

    public static final List<Pair<Regex, CalculatorToken>> TOKEN_LIST = List.of(
			new Pair<>(new Regex("\\+"), PLUS),
			new Pair<>(new Regex("-"), MINUS),
			new Pair<>(new Regex("\\*"), MULT),
			new Pair<>(new Regex("/"), DIV),
			new Pair<>(new Regex("\\("), LP),
			new Pair<>(new Regex("\\)"), RP),
			new Pair<>(new Regex("[0-9]+[.][0-9]+"), FLOAT),
			new Pair<>(new Regex("[0-9]+"), INT),
			new Pair<>(new Regex("_EPS_"), EPS)
    );
}
