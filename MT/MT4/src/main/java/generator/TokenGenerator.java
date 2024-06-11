package generator;

import state.GrammarData;

import java.io.File;

import static generator.Utility.createDirectoryIfDoesntExist;

public class TokenGenerator {

    public void createTokenClass(GrammarData data, String path, String packageName) {
        createDirectoryIfDoesntExist(path);

        String className = data.getName() + "Token";
        String writePath = path + className + ".java";

        StringBuilder enumTextBuilder = new StringBuilder();
        StringBuilder compObjectBuilder = new StringBuilder();

        for (state.TerminalState term : data.getTerminalList()) {
            String currName = term.getName();
            String currValue = term.getValue();
            enumTextBuilder.append("\t")
                    .append(currName)
                    .append("(\"")
                    .append(currName)
                    .append("\", \"")
                    .append(currValue)
                    .append("\", new Regex(\"")
                    .append(currValue)
                    .append("\")),\n");

            compObjectBuilder.append("\t\t\tnew Pair<>(new Regex(\"").
                    append(currValue)
                    .append("\"), ")
                    .append(currName)
                    .append("),\n");
        }

        String enumText = enumTextBuilder.toString();
        String compObject = compObjectBuilder.toString();

        int lastCommaIndex = compObject.lastIndexOf(',');

        if (lastCommaIndex != -1) {
            compObject = compObject.substring(0, lastCommaIndex) + compObject.substring(lastCommaIndex + 1);
        }

        String fileText = "package " + packageName + ";\n\n" +
                "import java.util.Map;\n" +
                "import java.util.List;\n" +
                "import kotlin.Pair;\n" +
                "import kotlin.text.Regex;\n\n" +
                "public enum " + className + " {\n" +
                enumText +
                "    END(\"END\",\"\",new Regex(\"\"));\n" +
                "\n" +
                "    public String termName = \"\";\n" +
                "    public String termValue = \"\";\n" +
                "    public final Regex termRegexp;\n" +
                "\n" +
                "    " + className + "(String termName, String termValue, Regex termRegexp) {\n" +
                "        this.termName = termName;\n" +
                "        this.termValue = termValue;\n" +
                "        this.termRegexp = termRegexp;\n" +
                "    }\n" +
                "\n" +
                "    public static final List<Pair<Regex, " + className + ">> TOKEN_LIST = List.of(\n" +
                compObject +
                "    );\n" +
                "}\n";

        try {
            File file = new File(writePath);
            file.createNewFile();
            java.nio.file.Files.write(file.toPath(), fileText.getBytes());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
