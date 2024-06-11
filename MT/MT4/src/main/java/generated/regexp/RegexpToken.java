package generated.regexp;

import java.util.Map;
import java.util.List;
import kotlin.Pair;
import kotlin.text.Regex;

public enum RegexpToken {
	PIPE("PIPE", "\\|", new Regex("\\|")),
	AST("AST", "\\*", new Regex("\\*")),
	LP("LP", "\\(", new Regex("\\(")),
	RP("RP", "\\)", new Regex("\\)")),
	VAR("VAR", "[a-z]", new Regex("[a-z]")),
	EPS("EPS", "_EPS_", new Regex("_EPS_")),
    END("END","",new Regex(""));

    public String termName = "";
    public String termValue = "";
    public final Regex termRegexp;

    RegexpToken(String termName, String termValue, Regex termRegexp) {
        this.termName = termName;
        this.termValue = termValue;
        this.termRegexp = termRegexp;
    }

    public static final List<Pair<Regex, RegexpToken>> TOKEN_LIST = List.of(
			new Pair<>(new Regex("\\|"), PIPE),
			new Pair<>(new Regex("\\*"), AST),
			new Pair<>(new Regex("\\("), LP),
			new Pair<>(new Regex("\\)"), RP),
			new Pair<>(new Regex("[a-z]"), VAR),
			new Pair<>(new Regex("_EPS_"), EPS)
    );
}
