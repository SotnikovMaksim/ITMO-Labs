package state;

import java.util.ArrayList;
import java.util.List;

public class NonTerminalState {
    public String name = "";
    public String argName = "";
    public String argClass = "";
    public List<RuleTarget> targets = new ArrayList<>();

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getArgName() {
        return argName;
    }

    public void setArgName(String argName) {
        this.argName = argName;
    }

    public String getArgClass() {
        return argClass;
    }

    public void setArgClass(String argClass) {
        this.argClass = argClass;
    }

    public List<RuleTarget> getTargets() {
        return targets;
    }

}
