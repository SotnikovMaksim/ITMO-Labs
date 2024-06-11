package state;

import java.util.ArrayList;
import java.util.List;

public class RuleTarget {
    public List<NonTerminalTarget> statesList = new ArrayList<>();

    public List<NonTerminalTarget> getStatesList() {
        return statesList;
    }
}
