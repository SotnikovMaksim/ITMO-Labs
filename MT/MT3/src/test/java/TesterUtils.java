import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import utils.CodeOutputUtil;
import utils.FileInputUtil;
import utils.FileOutputUtil;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;

public class TesterUtils {

    private static final Charset USING_CHARSET = StandardCharsets.UTF_8;

    protected static final String RESOURCES_PATH = "/Users/mac/JetBrains_Projects/IdeaProjects/MT3/src/test/resources";

    public static void translateFromSource(Path sourcePath, Path resultPath, String javaClassName) throws IOException {
        CharStream codePointCharStream = FileInputUtil.loadFileFromResources(sourcePath.toString(), USING_CHARSET);

        // Instantiate lexer and parser
        FuncLangLexer lexer = new FuncLangLexer(codePointCharStream);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        FuncLangParser parser = new FuncLangParser(tokens);

        // Parse the source code and get the parse tree
        ParseTree tree = parser.program();

        FuncLangToJavaVisitor visitor = new FuncLangToJavaVisitor();
        String translatedCode = visitor.visit(tree);

        String completeJavaCode = CodeOutputUtil.createJavaClassFromCode(translatedCode, javaClassName);

        writeToFile(completeJavaCode, resultPath);
    }

    private static void writeToFile(String translatedCode, Path pathToFile) {
        try {
            FileOutputUtil.writeToFile(translatedCode, Path.of(RESOURCES_PATH).resolve(pathToFile));
            System.out.println("Generated Java code written to: " + pathToFile);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
