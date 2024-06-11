grammar GenericGrammar;

@header {
import state.*;
}

@members {
GrammarData grammarMember = new GrammarData();
}

read returns [GrammarData data]
@init {$data = new GrammarData();} :
    resName=grammarName {$data.setName($resName.resName);}
    grammarNodeValue[$data]
    resHeader=grammarHeader[$data]?
    (grammarTokens[$data])* EOF {$data.getTerminalList().add(new TerminalState("EPS", "_EPS_"));};

grammarName returns [String resName]: 'grammar ' GRAMMAR_NAME LINE_END {$resName = $GRAMMAR_NAME.text;};

grammarNodeValue [GrammarData data]: SPEC_LEFT typeVal=INPUT PIPE defVal=INPUT SPEC_RIGHT {
    String typeValeText = $typeVal.text;
    String defValText = $defVal.text;
    $data.setDefaultType(typeValeText.substring(1, typeValeText.length() - 1));
    $data.setDefaultArg(defValText.substring(1, defValText.length() - 1));
};

grammarHeader [GrammarData data]: HEADER codeData=functionCode {$data.setHeader($codeData.codeData);};

grammarTokens [GrammarData data]: (
        term=grammarTerminal {$data.getTerminalList().add($term.state);}
        | nonTerm=grammarNonTerminal {$data.getNonTerminalList().add($nonTerm.state);}
    ) LINE_END;

grammarNonTerminal returns [NonTerminalState state]
@init {$state = new NonTerminalState();} :
    NON_TERMINAL_NAME {$state.setName($NON_TERMINAL_NAME.text);}
    (terminalVariable[$state])?
    DOUBLE_POINT allTargets[$state];

allTargets [NonTerminalState state]: targets[$state];

targets [NonTerminalState state]
@init {RuleTarget target = new RuleTarget();} :
    element[target]+ {$state.getTargets().add(target);} targetsPoint[$state];

targetsPoint [NonTerminalState state]
@init {RuleTarget target = new RuleTarget();} :
    | PIPE element[target]+ {$state.getTargets().add(target);} targetsPoint[$state];

element [RuleTarget target]
@init {NonTerminalTarget nonTermtarget = new NonTerminalTarget();} :
    ( TERMINAL_NAME {nonTermtarget.setName($TERMINAL_NAME.text);}
    | NON_TERMINAL_NAME {nonTermtarget.setName($NON_TERMINAL_NAME.text);} terminalParameter[nonTermtarget]?
    ) targetCode[nonTermtarget]? {$target.getStatesList().add(nonTermtarget);};

targetCode [NonTerminalTarget target]: code=functionCode {$target.setCode($code.codeData);};

terminalVariable [NonTerminalState state]:
    SPEC_LEFT className=GRAMMAR_NAME varName=NON_TERMINAL_NAME SPEC_RIGHT {
        $state.setArgName($varName.text);
        $state.setArgClass($className.text);
    };

grammarTerminal returns [TerminalState state]: TERMINAL_NAME DOUBLE_POINT INPUT {
    String valueText = $INPUT.text;
    $state = new TerminalState($TERMINAL_NAME.text, valueText.substring(1, valueText.length() - 1));
};

terminalParameter[NonTerminalTarget target] : FUNCTION_PARAMETER {
    String valueText = $FUNCTION_PARAMETER.text;
    $target.setParameter(valueText.substring(1, valueText.length() - 1));
};

functionCode returns [String codeData]:
    FUNCTION_CODE {
        String inpData = $FUNCTION_CODE.text;
        if (inpData == null) $codeData = "";
        else $codeData = inpData.substring(1, inpData.length() - 1);
    };

INPUT: '\'' .*? '\'';
EQUAL: '=';
LINE_END: ';';
DOUBLE_POINT: ':';
PIPE: '|';
ARROW: '->';
PARAM_LEFT: '[';
PARAM_RIGHT: ']';
FUNCTION_LEFT: '{';
FUNCTION_RIGHT: '}';
SPEC_LEFT: '<';
SPEC_RIGHT: '>';
VAR: 'var';
HEADER: '@header';
TERMINAL_NAME: [A-Z]+[A-Z_]*;
GRAMMAR_NAME: [A-Z]+[a-zA-Z]*;
NON_TERMINAL_NAME: [a-z]+[a-zA-Z]*;
FUNCTION_PARAMETER: PARAM_LEFT .+? PARAM_RIGHT;
FUNCTION_CODE: FUNCTION_LEFT .*? FUNCTION_RIGHT;

WS: [ \r\t\n]+ -> skip;
