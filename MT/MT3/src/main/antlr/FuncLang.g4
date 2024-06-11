grammar FuncLang;

program:   (functionDef | expr)* ;

// Function definition
functionDef: '(' 'defn' IDENTIFIER '(' IDENTIFIER* ')' block ')' ;

// Block of expressions (e.g., for function bodies)
block: expr+ ;

// Expressions
expr:   '(' functionLikeExpr ')'
      | '(' operation ')'
      | list
      | literal
      | IDENTIFIER ;

return: 'return' expr ;

functionLikeExpr:  ifExpr
             | print
             | varDef
             | return
             | functionCall; // Other cases

// Function call
functionCall: IDENTIFIER expr* ;

// Simple print in console
print: 'print' expr ;

// If expression
ifExpr: 'if' expr expr (expr)? ;

// Variable definition (e.g., 'def')
varDef: 'def' IDENTIFIER expr ;

// Unary operators
unaryOp: arithUnaryOp | boolUnaruOp ;

arithUnaryOp: ( 'neg' ) ;

boolUnaruOp: ( 'not' ) ;

// Binary operators
binOp: arithBinOp | boolBinOp ;

arithBinOp: ( '+' | '-' | '*' | '/' | '%' ) ;

boolBinOp: ( '<' | '<=' | '==' | '>=' | '>' ) ;

// Arithmetic operations
operation: binOp expr expr | unaryOp expr ;

// List (e.g., '[ 1 2 3 ]')
list: '[' expr* ']' ;

// Literals
literal: FLOAT | INT | STRING ;

// Lexical rules
IDENTIFIER: [a-zA-Z]+ [0-9]*;  // Identifiers
INT:        [0-9]+ ;           // Integer numbers
FLOAT:      [0-9]+ '.' [0-9]+; // Float numbers
STRING:     '"' .*? '"' ;      // String literals

// Skipping whitespaces and newlines
WS: [ \t\r\n]+ -> skip ;
