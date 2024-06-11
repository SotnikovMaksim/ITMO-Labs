package generator;

import exception.GrammarException;
import state.GrammarData;
import state.NonTerminalState;
import state.NonTerminalTarget;
import state.RuleTarget;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class FirstFollowSetsGenerator {

    public final GrammarData grammarData;

    public final HashMap<String, HashSet<String>> firstMap = new HashMap<>();

    public final HashMap<String, HashSet<String>> followMap = new HashMap<>();

    public FirstFollowSetsGenerator(GrammarData grammarData) throws GrammarException {
        this.grammarData = grammarData;

        initMaps();
        generateFirstSet();
        generateFollowSet();

        checkLL1();
    }

    private void checkLL1() throws GrammarException {
        boolean LL1 = true;

        outerloop:
        for (NonTerminalState nonTerm : grammarData.getNonTerminalList()) {
            for (RuleTarget alphaRule : nonTerm.getTargets()) {
                if (!alphaRule.getStatesList().isEmpty()) {
                    NonTerminalTarget alpha = alphaRule.getStatesList().get(0);

                    if (!firstMap.containsKey(alpha.getName())) {
                        continue;
                    }

                    List<RuleTarget> betaRules = new ArrayList<>(nonTerm.getTargets());
                    betaRules.remove(alphaRule);

                    for (RuleTarget betaRule : betaRules) {
                        if (!betaRule.getStatesList().isEmpty()) {
                            NonTerminalTarget beta = betaRule.getStatesList().get(0);

                            if (!firstMap.containsKey(beta.getName())) {
                                continue;
                            }

                            if (alpha.getName().equals(beta.getName())) {
                                continue;
                            }

                            Set<String> firstSetAlpha = new HashSet<>(firstMap.get(alpha.getName()));
                            Set<String> firstSetBeta = new HashSet<>(firstMap.get(beta.getName()));
                            firstSetAlpha.retainAll(firstSetBeta);

                            if (!firstSetAlpha.isEmpty()) {
                                LL1 = false;
                                break outerloop;
                            }
                        }
                    }
                }
            }
        }

        if (LL1) {
            outerloop:
            for (NonTerminalState nonTerm : grammarData.getNonTerminalList()) {
                for (RuleTarget alphaRule : nonTerm.getTargets()) {
                    if (!alphaRule.getStatesList().isEmpty()) {
                        NonTerminalTarget alpha = alphaRule.getStatesList().get(0);

                        if (!firstMap.containsKey(alpha.getName())) {
                            continue;
                        }

                        List<RuleTarget> betaRules = new ArrayList<>(nonTerm.getTargets());
                        betaRules.remove(alphaRule);

                        for (RuleTarget betaRule : betaRules) {
                            if (!betaRule.getStatesList().isEmpty()) {
                                NonTerminalTarget beta = betaRule.getStatesList().get(0);

                                if (!firstMap.containsKey(beta.getName())) {
                                    continue;
                                }

                                if (alpha.getName().equals(beta.getName())) {
                                    continue;
                                }

                                if (firstMap.get(alpha.getName()).contains(_EPS)) {
                                    Set<String> followSetA = new HashSet<>(followMap.get(nonTerm.getName()));
                                    Set<String> firstSetBeta = new HashSet<>(followMap.get(beta.getName()));

                                    followSetA.retainAll(firstSetBeta);

                                    if (!followSetA.isEmpty()) {
                                        LL1 = false;
                                        break outerloop;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        if (!LL1) {
            throw new GrammarException("Provided grammar is not LL(1)");
        }
    }

    private void initMaps() {
        for (NonTerminalState nonTerm : grammarData.getNonTerminalList()) {
            firstMap.put(nonTerm.getName(), new HashSet<>());
            followMap.put(nonTerm.getName(), new HashSet<>());
        }
    }

    private void generateFirstSet() {
        boolean changed = true;

        while (changed) {
            changed = false;

            for (NonTerminalState nonTerm : grammarData.getNonTerminalList()) {
                for (RuleTarget nonTermRules : nonTerm.getTargets()) {
                    if (!nonTermRules.getStatesList().isEmpty()) {
                        NonTerminalTarget firstFromTarget = nonTermRules.getStatesList().get(0);
                        List<String> pushedData = getFirst(firstFromTarget);

                        int sizeBefore = firstMap.get(nonTerm.getName()).size();

                        firstMap.get(nonTerm.getName()).addAll(pushedData);

                        changed = changed || (sizeBefore != firstMap.get(nonTerm.getName()).size());
                    }
                }
            }
        }
    }

    private void generateFollowSet() {
        boolean changed = true;

        if (!grammarData.getNonTerminalList().isEmpty()) {
            followMap.get(grammarData.getNonTerminalList().get(0).getName()).add(_DOLLAR);
        }

        while (changed) {
            changed = false;

            for (NonTerminalState nonTerm : grammarData.getNonTerminalList()) {
                for (RuleTarget nonTermRules : nonTerm.getTargets()) {
                    int stateSize = nonTermRules.getStatesList().size();

                    for (int index = 0; index < stateSize; index++) {
                        String beta = nonTermRules.getStatesList().get(index).getName();

                        if (grammarData.getTerminalList().stream().noneMatch(terminal -> terminal.getName().equals(beta))) {
                            String gamma = (index == stateSize - 1) ? _EPS : nonTermRules.getStatesList().get(index + 1).getName();

                            List<String> firstData = getFirst(gamma);
                            if (firstData.contains(_EPS)) {
                                firstData.addAll(followMap.get(nonTerm.getName()));
                            }
                            firstData.removeIf(s -> s.equals(_EPS));

                            int sizeBefore = followMap.get(beta).size();
                            followMap.get(beta).addAll(firstData);

                            changed = changed || (sizeBefore != followMap.get(beta).size());
                        }
                    }
                }
            }
        }
    }

    private List<String> getFirst(String token) {
        List<String> pushedData = new ArrayList<>();

//      Check if provided token is terminal
        if (grammarData.getTerminalList().stream().anyMatch(terminal -> terminal.getName().equals(token))) {
            pushedData.add(token);
        } else {
            pushedData.addAll(firstMap.get(token));
        }

        return pushedData;
    }

    private List<String> getFirst(NonTerminalTarget target) {
        String name = target.getName();
        return getFirst(name);
    }

    public static final String _EPS = "EPS";
    public static final String _DOLLAR = "$";
}
