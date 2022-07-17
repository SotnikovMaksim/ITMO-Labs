package markup;

import java.lang.StringBuilder;

public interface MD {
    void toMarkdown(StringBuilder in);

    void toBBCode(StringBuilder in);
}
