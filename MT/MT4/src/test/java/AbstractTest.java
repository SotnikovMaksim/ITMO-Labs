import exception.GrammarException;
import generator.FirstFollowSetsGenerator;
import generator.LexerGenerator;
import generator.ParserGenerator;
import generator.TokenGenerator;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

import java.io.IOException;
import java.text.ParseException;

public abstract class AbstractTest<T> {

    public static void generateParser(String parserName) throws IOException, GrammarException {
        String pathForGenerated = "src/main/java/generated/" + parserName.toLowerCase() + "/";
        String packageOfGenerated = "generated." + parserName.toLowerCase();

        GenericGrammarParser.ReadContext grammar = createContext(parserName);

        new TokenGenerator().createTokenClass(grammar.data, pathForGenerated, packageOfGenerated);
        new LexerGenerator().createLexerClass(grammar.data, pathForGenerated, packageOfGenerated);

        new ParserGenerator(new FirstFollowSetsGenerator(grammar.data))
                .createParserClass(grammar.data, pathForGenerated, packageOfGenerated);
    }

    private static GenericGrammarParser.ReadContext createContext(String grammarFileName) throws IOException {
        GenericGrammarParser pars = new GenericGrammarParser(
                new CommonTokenStream(
                        new GenericGrammarLexer(
                                CharStreams.fromFileName("src/main/resources/grammar/" + grammarFileName))));

        return pars.read();
    }

    protected abstract void abstractTest(String testLine, T expected) throws ParseException;
}
