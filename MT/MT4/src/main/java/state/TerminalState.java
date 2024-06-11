package state;

public class TerminalState {
    public String name;
    public String value;

    public TerminalState(String name, String value) {
        this.name = name;
        this.value = value;
    }

    public String getName() {
        return name;
    }

    public String getValue() {
        return value;
    }
}
