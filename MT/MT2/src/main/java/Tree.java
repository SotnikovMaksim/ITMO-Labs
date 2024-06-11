import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

public class Tree {

    public static final Set<String> grammarTerms = Set.of(
            "E",
            "P",
            "P'",
            "A",
            "A'",
            "S"
    );

    private final String node;

    private final List<Tree> children;

    public Tree(String node, Tree... children) {
        this.node = node;
        this.children = Arrays.asList(children);
    }

    public Tree(String node) {
        this(node, new Tree[0]);
    }

    public static void toRegExp(Tree tree, StringBuilder sb) {
        for (Tree child : tree.children) {
            if (!grammarTerms.contains(child.node)) {
                sb.append(child.node);

            }
            toRegExp(child, sb);
        }
    }

    public List<Tree> getChildren() {
        return children;
    }

    public String getNode() {
        return node;
    }

    public static void printTree(Tree root) {
        List<List<String>> rows = new ArrayList<>();
        List<Tree> nextLevel = new ArrayList<>();
        nextLevel.add(root);

        int size = 1;
        while (size > 0) {
            size = 0;
            List<String> row = new ArrayList<>();
            rows.add(row);

            List<Tree> nextLevelTmp = new ArrayList<>();
            for (Tree next : nextLevel) {
                if (next == null) {
                    row.add(null);
                    nextLevelTmp.add(null);
                    nextLevelTmp.add(null);
                    continue;
                }

                row.add(next.node);

                for (Tree nextChild : next.children) {
                    if (nextChild != null) {
                        size++;
                    }
                    nextLevelTmp.add(nextChild);
                }
            }

            nextLevel = nextLevelTmp;
        }

        // Ensure we have enough space for the leaf nodes.
        int minPartitionSize = 2;
        int maxSize = minPartitionSize * (int) Math.pow(2, rows.size());

        String[][] rowArrs = new String[rows.size()][maxSize];
        int rowNum = 0;
        for (List<String> row : rows) {

            StringBuilder barText = new StringBuilder(maxSize);
            StringBuilder dataText = new StringBuilder(maxSize);

            int divisions = (int) Math.pow(2, rowNum);
            int divisionSize = maxSize / divisions;
            int idx = 0;
            for (String val : row) {
                // Build empty partition slot for the row
                char[] data = new char[divisionSize];
                Arrays.fill(data, ' ');
                char[] bar = new char[divisionSize];
                Arrays.fill(bar, ' ');

                if (val != null) {
                    if (rowNum != 0) {
                        // Add connecting pipe based on if it is a left or right child.
                        if (idx % 2 == 0) {
                            bar[divisionSize * 3 / 4] = '/';
                        } else {
                            bar[divisionSize / 4] = '\\';
                        }
                    }

                    // Center the values text around the partitions center
                    char[] valText = val.toCharArray();
                    int midpoint = divisionSize / 2;
                    if (valText.length > divisionSize - 2) {
                        throw new RuntimeException("Input " + val + " has too many digits to fit in partition");
                    }
                    System.arraycopy(valText, 0, data, midpoint - (valText.length / 2), valText.length);
                }
                barText.append(bar);
                dataText.append(data);
                idx++;
            }
            System.out.println(barText);
            System.out.println(dataText);
            rowNum++;
        }
    }
}
