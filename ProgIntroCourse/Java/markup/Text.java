package markup;

import java.lang.StringBuilder;

public class Text implements MD {
    private final String text;

    public Text(String in) {
        this.text = in;
    }

    public void toMarkdown(StringBuilder in) {
        in.append(this.text);
    }

    public void toBBCode(StringBuilder in) {
        in.append(this.text);
    }
}
