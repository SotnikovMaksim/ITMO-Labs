package markup;

import java.lang.StringBuilder;
import java.util.List;

public class Strong extends Paragraph implements MD {
    public Strong(List<MD> source)  {
        super(source);
    }

    public void toMarkdown(StringBuilder in) {
        in.append("__");
        for (MD md : this.list) {
            md.toMarkdown(in);
        }
        in.append("__");
    }

    public void toBBCode(StringBuilder in) {
        in.append("[b]");
        for (MD md : this.list) {
            md.toBBCode(in);
        }
        in.append("[/b]");
    }
}
