import exception.GrammarException;
import generator.FirstFollowSetsGenerator;
import generator.LexerGenerator;
import generator.ParserGenerator;
import generator.TokenGenerator;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

import java.io.IOException;

public class Main {

    public static void main(String[] args) throws IOException, GrammarException {
//        generateCalculatorParser();
//        generateRegexpParser();
        generateBadGrammarParser();
    }

    private static void generateCalculatorParser() throws IOException, GrammarException {
        String grammarFileName = "Calculator";
        String pathForGenerated = "src/main/java/generated/calculator/";
        String packageOfGenerated = "generated.calculator";

        GenericGrammarParser.ReadContext grammar = createContext(grammarFileName);

        new TokenGenerator().createTokenClass(grammar.data, pathForGenerated, packageOfGenerated);
        new LexerGenerator().createLexerClass(grammar.data, pathForGenerated, packageOfGenerated);

        new ParserGenerator(new FirstFollowSetsGenerator(grammar.data))
                .createParserClass(grammar.data, pathForGenerated, packageOfGenerated);
    }

    private static void generateRegexpParser() throws IOException, GrammarException {
        String grammarFileName = "Regexp";
        String pathForGenerated = "src/main/java/generated/regexp/";
        String packageOfGenerated = "generated.regexp";

        GenericGrammarParser.ReadContext grammar = createContext(grammarFileName);

        new TokenGenerator().createTokenClass(grammar.data, pathForGenerated, packageOfGenerated);
        new LexerGenerator().createLexerClass(grammar.data, pathForGenerated, packageOfGenerated);

        new ParserGenerator(new FirstFollowSetsGenerator(grammar.data))
                .createParserClass(grammar.data, pathForGenerated, packageOfGenerated);
    }

    private static void generateBadGrammarParser() throws IOException, GrammarException {
        String grammarFileName = "Bad";
        String pathForGenerated = "src/main/java/generated/bad/";
        String packageOfGenerated = "generated.bad";

        GenericGrammarParser.ReadContext grammar = createContext(grammarFileName);

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
}


