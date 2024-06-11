package generator;

import state.GrammarData;

import java.io.File;
import java.nio.file.Files;
import java.text.ParseException;

import static generator.Utility.createDirectoryIfDoesntExist;

public class LexerGenerator {

    public void createLexerClass(GrammarData data, String path, String packageName) {
        createDirectoryIfDoesntExist(path);

        String lexerName = data.getName() + "Lexer";
        String tokenName = data.getName() + "Token";
        String writePath = path + lexerName + ".java";

        StringBuilder regTextBuilder = new StringBuilder();
        for (state.TerminalState term : data.getTerminalList()) {
            String currValue = term.getValue();
            regTextBuilder.append(currValue).append("|");
        }

        String regText = regTextBuilder.toString();
        if (!regText.isEmpty()) {
            regText = regText.substring(0, regText.length() - 1);
        }

        String fileText = "package " + packageName + ";\n\n" +
                "import java.text.ParseException;\n" +
                "import kotlin.text.Regex;\n" +
                "import kotlin.text.MatchResult;\n\n" +
                "public class " + lexerName + " {\n" +
                "\n" +
                "    private final String text;\n" +
                "    private final Regex tokenRegex = new Regex(\"" + regText + "\");\n" +
                "\n" +
                "    private " + tokenName + " tokens;\n" +
                "    private MatchResult matcher;\n" +
                "    private int pos;\n" +
                "    private " + tokenName + " currToken;\n" +
                "\n" +
                "    public " + lexerName + "(String text) throws ParseException {\n" +
                "        this.text = text;\n" +
                "        this.matcher = tokenRegex.find(text, 0);\n" +
                "        nextToken();\n" +
                "    }\n" +
                "\n" +
                "    public " + tokenName + " nextToken() throws ParseException {\n" +
                "        if (matcher == null) {\n" +
                "           currToken = " + tokenName + ".END;\n" +
                "           return " + tokenName + ".END;\n" +
                "        }\n" +
                "        while (pos < text.length() && Character.isWhitespace(text.charAt(pos))) pos++;\n" +
                "        if (matcher.getRange().getStart() != pos) throw new ParseException(text, pos);\n" +
                "\n" +
                "        String currentValue = matcher.getValue();\n" +
                "        for (var checkedToken : " + tokenName + ".TOKEN_LIST) {\n" +
                "            if (checkedToken.getFirst().matches(currentValue)) {\n" +
                "                String matchedText = matcher.getValue();\n" +
                "                pos += currentValue.length();\n" +
                "                matcher = matcher.next();\n" +
                "                currToken = Enum.valueOf(" + tokenName + ".class, checkedToken.getSecond().termName);\n" +
                "                currToken.termValue = matchedText;\n" +
                "                return currToken;\n" +
                "            }\n" +
                "        }\n" +
                "        throw new ParseException(text, pos);\n" +
                "    }\n" +
                "\n" +
                "    public " + tokenName + " currentToken() {\n" +
                "        return currToken;\n" +
                "    }\n" +
                "\n" +
                "    public int currentPos() {\n" +
                "        return pos;\n" +
                "    }\n" +
                "}";
        
        try {
            File file = new File(writePath);
            file.createNewFile();
            Files.write(file.toPath(), fileText.getBytes());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
