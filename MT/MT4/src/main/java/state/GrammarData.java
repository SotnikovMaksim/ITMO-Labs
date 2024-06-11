package state;

import java.util.ArrayList;
import java.util.List;

public class GrammarData {

    public String name = "";
    public String header = "";
    public String defaultType = "";
    public String defaultArg = "";
    public List<TerminalState> terminalList = new ArrayList<>();
    public List<NonTerminalState> nonTerminalList = new ArrayList<>();

    public void setName(String name) {
        this.name = name;
    }

    public void setHeader(String header) {
        this.header = header;
    }

    public void setDefaultType(String defaultType) {
        this.defaultType = defaultType;
    }

    public void setDefaultArg(String defaultArg) {
        this.defaultArg = defaultArg;
    }

    public String getName() {
        return name;
    }

    public String getHeader() {
        return header;
    }

    public String getDefaultType() {
        return defaultType;
    }

    public String getDefaultArg() {
        return defaultArg;
    }

    public List<TerminalState> getTerminalList() {
        return terminalList;
    }

    public List<NonTerminalState> getNonTerminalList() {
        return nonTerminalList;
    }
}
