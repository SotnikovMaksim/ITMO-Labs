package markup;

import java.util.List;
import java.lang.StringBuilder;

public class Paragraph implements MD {
    protected List<MD> list;

    public Paragraph(List<MD> source) {
        this.list = source;
    }

    @Override
    public void toMarkdown(StringBuilder in) {
        for (MD md : this.list) {
            md.toMarkdown(in);
        }
    }

    @Override
    public void toBBCode(StringBuilder in) {
        for (MD md : this.list) {
            md.toBBCode(in);
        }
    }
}
