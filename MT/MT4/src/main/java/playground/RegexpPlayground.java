package playground;

import generated.regexp.Node;
import generated.regexp.RegexpLexer;
import generated.regexp.RegexpParser;

import java.text.ParseException;

public class RegexpPlayground {

    public static void main(String[] args) throws ParseException {
        String line = "a(b";
        RegexpLexer lexer = new RegexpLexer(line);
        Node node = new RegexpParser(lexer).eTerm();

        System.out.println(node.value);
    }
}
