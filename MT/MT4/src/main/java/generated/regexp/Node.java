package generated.regexp;

import java.util.ArrayList;
import java.util.List;

public class Node {
    public String tokenName;
    public String value = "";
    public List<Node> tokenChildren = new ArrayList<>();
    public String tokenValue = "";

    public Node(String tokenName, String tokenValue) {
        this.tokenName = tokenName;
        this.tokenValue = tokenValue;
    }
}
