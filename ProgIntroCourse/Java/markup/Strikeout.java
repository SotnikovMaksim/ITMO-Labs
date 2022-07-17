package markup;

import java.lang.StringBuilder;
import java.util.List;

public class Strikeout extends Paragraph implements MD {
    public Strikeout(List<MD> source)  {
       super(source);
    }

    public void toMarkdown(StringBuilder in) {
        in.append("~");
        for (MD md : this.list) {
            md.toMarkdown(in);
        }
        in.append("~");
    }

    public void toBBCode(StringBuilder in) {
        in.append("[s]");
        for (MD md : this.list) {
            md.toBBCode(in);
        }
        in.append("[/s]");
    }
}
