import exception.ParseException;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.Scanner;

public class Main {

    private static final String GRAPHVIZ_TEMPLATE = """
            digraph G {
              node [shape=plain, fontname="Arial"];
                       
            %s
            }
            """;

    private static Integer literalCounter = 1;

    public static void main(String[] args) throws ParseException, IOException {
        final Scanner in = new Scanner(System.in);
        final String input = in.nextLine();

        final InputStream is = new ByteArrayInputStream(input.getBytes());
        final Parser parser = new Parser();

        final Tree tree = parser.parse(is);

        StringBuilder sb = new StringBuilder();
        treeToGraphvizConfig(tree, sb,
                Map.of(
                        "A", new MutableInt(),
                        "E", new MutableInt(),
                        "S", new MutableInt(),
                        "P", new MutableInt(),
                        "P'", new MutableInt(),
                        "A'", new MutableInt(),
                        "_", new MutableInt(),
                        "|", new MutableInt(),
                        "*", new MutableInt()
                )
        );
        String config = GRAPHVIZ_TEMPLATE.formatted(sb.toString())
                .replace("'", "term")
                .replace("|", "vert")
                .replace("_", "eps");

        String fileName = "example";
        Files.write(Path.of(fileName + ".dot"), config.getBytes());

        compileGraphviz("example");
    }

    private static void treeToGraphvizConfig(Tree tree, StringBuilder sb, Map<String, MutableInt> counter) {
        String currentNodeName;
        if (counter.containsKey(tree.getNode())) {
            var nameCount = counter.get(tree.getNode());
            currentNodeName = tree.getNode() + nameCount.get();
            nameCount.increment();
        } else {
            currentNodeName = tree.getNode() + literalCounter++;
        }

        for (Tree child : tree.getChildren()) {
            String childNodeName;
            if (counter.containsKey(child.getNode())) {
                var nameCount = counter.get(child.getNode());
                childNodeName = child.getNode() + nameCount.get();
            } else {
                childNodeName = child.getNode() + literalCounter++;
            }

            sb.append("    ")
                    .append(currentNodeName)
                    .append(" -> ")
                    .append(childNodeName)
                    .append(";")
                    .append(System.lineSeparator());
            treeToGraphvizConfig(child, sb, counter);
        }
    }

    private static void compileGraphviz(String fileName) {
        final ProcessBuilder pb = new ProcessBuilder("dot", "-Tsvg", fileName + ".dot", "-o", fileName + ".svg");
        try {
            final Process p = pb.start();
            p.waitFor();
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
    }

    static class MutableInt {
        int value = 1; // note that we start at 1 since we're counting
        public void increment () { ++value;      }
        public int  get ()       { return value; }
    }
}
