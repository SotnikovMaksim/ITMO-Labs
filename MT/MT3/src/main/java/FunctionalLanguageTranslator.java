import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import utils.CodeOutputUtil;
import utils.FileInputUtil;
import utils.FileOutputUtil;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;

public class FunctionalLanguageTranslator {

    private static final Charset USING_CHARSET = StandardCharsets.UTF_8;
    private static final String NAME_OF_SRC = "functionLangSrc.testlang";

    public static void main(String[] args) throws Exception {
        CharStream codePointCharStream = FileInputUtil.loadFileFromResources(NAME_OF_SRC, USING_CHARSET);

        // Instantiate lexer and parser
        FuncLangLexer lexer = new FuncLangLexer(codePointCharStream);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        FuncLangParser parser = new FuncLangParser(tokens);

        // Parse the source code and get the parse tree
        ParseTree tree = parser.program();

        FuncLangToJavaVisitor visitor = new FuncLangToJavaVisitor();
        String translatedCode = visitor.visit(tree);

        String javaClassName = "TranslatedClass"; // Result class name
        String completeJavaCode = CodeOutputUtil.createJavaClassFromCode(translatedCode, javaClassName);

        // Output the complete Java class code
        System.out.println("RESULT:");
        System.out.println(completeJavaCode);

        writeToFile(completeJavaCode);
    }

    private static void writeToFile(String translatedCode) {
        try {
            String outputPath = "/Users/mac/JetBrains_Projects/IdeaProjects/MT3/src/main/java/output/TranslatedClass.java";
            FileOutputUtil.writeToFile(translatedCode, Path.of(outputPath));
            System.out.println("Generated Java code written to: " + outputPath);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
