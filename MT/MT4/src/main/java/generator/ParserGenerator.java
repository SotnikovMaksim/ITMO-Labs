package generator;

import state.*;

import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static generator.Utility.createDirectoryIfDoesntExist;

public class ParserGenerator {

    private final String FUNCTION_NAME_SUFFIX = "Term";
    private final FirstFollowSetsGenerator firstAndFollow;

    public ParserGenerator(FirstFollowSetsGenerator firstAndFollow) {
        this.firstAndFollow = firstAndFollow;
    }

    public void createParserClass(GrammarData data, String path, String packageName) {
        generateNodeFile(data, path, packageName);
        generateParserFile(data, path, packageName);
    }

    private void generateNodeFile(GrammarData data, String path, String packageName) {
        createDirectoryIfDoesntExist(path);

        String writePath = path + "Node.java";

        String fileText = "package " + packageName + ";\n\n" +
                "import java.util.ArrayList;\n" +
                "import java.util.List;\n\n" +
                "public class Node {\n" +
                "    public String tokenName;\n" +
                "    public " + data.getDefaultType() + " value = " + data.getDefaultArg() + ";\n" +
                "    public List<Node> tokenChildren = new ArrayList<>();\n" +
                "    public String tokenValue = \"\";\n\n" +
                "    public Node(String tokenName, String tokenValue) {\n" +
                "        this.tokenName = tokenName;\n" +
                "        this.tokenValue = tokenValue;\n" +
                "    }\n" +
                "}\n";

        try {
            File file = new File(writePath);
            file.createNewFile();
            java.nio.file.Files.write(file.toPath(), fileText.getBytes());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void generateParserFile(GrammarData data, String path, String packageName) {
        createDirectoryIfDoesntExist(path);

        String lexerName = data.getName() + "Lexer";
        String tokenName = data.getName() + "Token";
        String parserName = data.getName() + "Parser";
        String writePath = path + parserName + ".java";

        StringBuilder fileTextBuilder = new StringBuilder();

        fileTextBuilder.append(getParserHead(data, lexerName, parserName, packageName));
        fileTextBuilder.append(getFunctionsBody(data, lexerName, parserName, tokenName)).append("}");

        try {
            File file = new File(writePath);
            file.createNewFile();
            java.nio.file.Files.write(file.toPath(), fileTextBuilder.toString().getBytes());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private String getParserHead(GrammarData data, String lexerName, String parserName, String packageName) {
        return "package " + packageName + ";\n\n" +
                data.getHeader() + "\n" +
                "import java.text.ParseException;\n\n" +
                "public class " + parserName + " {\n\n" +
                "    private final " + lexerName + " lexer;\n" +
                "\n" +
                "    public " + parserName + "(" + lexerName + " lexer) {\n" +
                "        this.lexer = lexer;\n" +
                "    }\n\n";
    }

    private String getFunctionsBody(GrammarData data, String lexerName, String parserName, String tokenName) {
        StringBuilder outputBodies = new StringBuilder();

        for (NonTerminalState nonTerminalState : data.getNonTerminalList()) {
            outputBodies.append(generateFunctionBody(data, nonTerminalState, tokenName));
        }
        return outputBodies + "";
    }

    private String generateFunctionBody(GrammarData data, NonTerminalState nonTerminalState, String tokenName) {
        String functionArg =
                (nonTerminalState.getArgName().isEmpty()) ? "" : nonTerminalState.getArgClass() + " " + nonTerminalState.getArgName();

        String resSetLine =
                (nonTerminalState.getArgName().isEmpty()) ? "" : "res.value = " + nonTerminalState.getArgName() + ";";

        StringBuilder stateInWhenLines = new StringBuilder();
        for (RuleTarget rule : nonTerminalState.getTargets()) {
            stateInWhenLines.append(getStateLines(data, nonTerminalState, rule, tokenName));
        }

        return "\tpublic Node " + nonTerminalState.getName() + FUNCTION_NAME_SUFFIX + "(" + functionArg + ") throws ParseException {\n" +
                "\t\tNode res = new Node(\"" + nonTerminalState.getName() + "\", lexer.currentToken().termValue);\n" +
                "\t\t" + resSetLine + "\n" +
                "\t\tswitch (lexer.currentToken()) {\n" +
                stateInWhenLines +
                "\t\t\tdefault -> throw new ParseException(\"Unexpected token: '\" + lexer.currentToken().termName + \"'\", lexer.currentPos());\n" +
                "\t\t}\n" +
                "\t\treturn res;\n" +
                "\t}\n\n";
    }

    private String getStateLines(GrammarData data, NonTerminalState nonTerminalState, RuleTarget targets, String tokenName) {
        String nextTargetName = targets.getStatesList().get(0).getName();
        Set<String> first = (data.getTerminalList().stream()
                .anyMatch(terminal -> terminal.getName().equals(nextTargetName))) ?
                new HashSet<>(Collections.singletonList(nextTargetName)) :
                firstAndFollow.firstMap.get(nextTargetName);

        Set<String> follow = firstAndFollow.followMap.get(nonTerminalState.getName());
        StringBuilder firstLines = new StringBuilder();
        Map<String, Set<String>> caseOutcomes = new HashMap<>();

        for (String currFirst : first) {
            if (currFirst.equals(FirstFollowSetsGenerator._EPS)) continue;
            String functionStateBodyRule = getFunctionStateBodyRule(data, targets);

            if (caseOutcomes.containsKey(functionStateBodyRule)) {
                caseOutcomes.get(functionStateBodyRule).add(currFirst);
            } else {
                Set<String> states = new HashSet<>();
                states.add(currFirst);
                caseOutcomes.put(functionStateBodyRule, states);
            }
//            firstLines.append("\t\t\tcase ")
//                    .append(currFirst)
//                    .append(" -> {\n")
//                    .append(functionStateBodyRule)
//                    .append("\t\t\t}\n");
        }

        follow.removeAll(first);
        if (first.contains(FirstFollowSetsGenerator._EPS)) {
            for (String currFollow : follow) {
                String currTokenName = (currFollow.equals(FirstFollowSetsGenerator._DOLLAR)) ? "END" : currFollow;
                String functionStateBodyRule = "";

                if (caseOutcomes.containsKey(functionStateBodyRule)) {
                    caseOutcomes.get(functionStateBodyRule).add(currTokenName);
                } else {
                    Set<String> states = new HashSet<>();
                    states.add(currTokenName);
                    caseOutcomes.put(functionStateBodyRule, states);
                }
//                firstLines.append("\t\t\tcase ")
//                        .append(currTokenName)
//                        .append(" -> {\n")
//                        .append(functionStateBodyRule)
//                        .append("\t\t\t}\n");
            }
        }

        for (var outcome : caseOutcomes.entrySet()) {
            firstLines.append("\t\t\tcase ")
                .append(String.join(", ", outcome.getValue()))
                .append(" -> {\n")
                .append(outcome.getKey())
                .append("\t\t\t}\n");
        }

        return firstLines.toString();
    }

    private String getFunctionStateBodyRule(GrammarData data, RuleTarget targets) {
        StringBuilder outputString = new StringBuilder();

        for (NonTerminalTarget target : targets.getStatesList()) {
            if (data.getTerminalList().stream().anyMatch(terminal -> terminal.getName().equals(target.getName()))) {
                outputString.append("\t\t\t\tNode ")
                        .append(target.getName())
                        .append(" = new Node(\"")
                        .append(target.getName())
                        .append("\", lexer.currentToken().termValue);\n");

                outputString.append("\t\t\t\tres.tokenChildren.add(")
                        .append(target.getName())
                        .append(");\n");

                outputString.append("\t\t\t\tlexer.nextToken();\n");

                outputString.append("\t\t\t\t")
                        .append(target.getCode())
                        .append("\n");
            } else {
                outputString.append("\t\t\t\tNode ")
                        .append(target.getName())
                        .append(" = ")
                        .append(target.getName())
                        .append(FUNCTION_NAME_SUFFIX)
                        .append("(")
                        .append(target.getParameter())
                        .append(");\n");

                outputString.append("\t\t\t\tres.tokenChildren.add(")
                        .append(target.getName())
                        .append(");\n");

                outputString.append("\t\t\t\t")
                        .append(target.getCode())
                        .append("\n");
            }
        }

        return outputString.toString();
    }
}
