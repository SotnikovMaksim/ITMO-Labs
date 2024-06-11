package generated.calculator;

import java.util.ArrayList;
import java.util.List;

public class Node {
    public String tokenName;
    public Double value = 0.0;
    public List<Node> tokenChildren = new ArrayList<>();
    public String tokenValue = "";

    public Node(String tokenName, String tokenValue) {
        this.tokenName = tokenName;
        this.tokenValue = tokenValue;
    }
}
