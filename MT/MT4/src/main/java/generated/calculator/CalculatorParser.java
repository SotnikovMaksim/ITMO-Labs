package generated.calculator;


import java.text.ParseException;

public class CalculatorParser {

    private final CalculatorLexer lexer;

    public CalculatorParser(CalculatorLexer lexer) {
        this.lexer = lexer;
    }

	public Node eTerm() throws ParseException {
		Node res = new Node("e", lexer.currentToken().termValue);
		
		switch (lexer.currentToken()) {
			case FLOAT, LP, INT, MINUS -> {
				Node t = tTerm();
				res.tokenChildren.add(t);
				
				Node eState = eStateTerm(t.value);
				res.tokenChildren.add(eState);
				res.value = eState.value;
			}
			default -> throw new ParseException("Unexpected token: '" + lexer.currentToken().termName + "'", lexer.currentPos());
		}
		return res;
	}

	public Node eStateTerm(Double count) throws ParseException {
		Node res = new Node("eState", lexer.currentToken().termValue);
		res.value = count;
		switch (lexer.currentToken()) {
			case PLUS -> {
				Node PLUS = new Node("PLUS", lexer.currentToken().termValue);
				res.tokenChildren.add(PLUS);
				lexer.nextToken();
				
				Node t = tTerm();
				res.tokenChildren.add(t);
				
				Node eState = eStateTerm(count + t.value);
				res.tokenChildren.add(eState);
				res.value = eState.value;
			}
			case MINUS -> {
				Node MINUS = new Node("MINUS", lexer.currentToken().termValue);
				res.tokenChildren.add(MINUS);
				lexer.nextToken();
				
				Node t = tTerm();
				res.tokenChildren.add(t);
				
				Node eState = eStateTerm(count - t.value);
				res.tokenChildren.add(eState);
				res.value = eState.value;
			}
			case DIV, MULT, END, RP -> {
			}
			default -> throw new ParseException("Unexpected token: '" + lexer.currentToken().termName + "'", lexer.currentPos());
		}
		return res;
	}

	public Node tTerm() throws ParseException {
		Node res = new Node("t", lexer.currentToken().termValue);
		
		switch (lexer.currentToken()) {
			case FLOAT, LP, INT, MINUS -> {
				Node f = fTerm();
				res.tokenChildren.add(f);
				
				Node tState = tStateTerm(f.value);
				res.tokenChildren.add(tState);
				res.value = tState.value;
			}
			default -> throw new ParseException("Unexpected token: '" + lexer.currentToken().termName + "'", lexer.currentPos());
		}
		return res;
	}

	public Node tStateTerm(Double count) throws ParseException {
		Node res = new Node("tState", lexer.currentToken().termValue);
		res.value = count;
		switch (lexer.currentToken()) {
			case MULT -> {
				Node MULT = new Node("MULT", lexer.currentToken().termValue);
				res.tokenChildren.add(MULT);
				lexer.nextToken();
				
				Node f = fTerm();
				res.tokenChildren.add(f);
				
				Node tState = tStateTerm(count * f.value);
				res.tokenChildren.add(tState);
				res.value = tState.value;
			}
			case DIV -> {
				Node DIV = new Node("DIV", lexer.currentToken().termValue);
				res.tokenChildren.add(DIV);
				lexer.nextToken();
				
				Node f = fTerm();
				res.tokenChildren.add(f);
				
				Node tState = tStateTerm(count / f.value);
				res.tokenChildren.add(tState);
				res.value = tState.value;
			}
			case END, RP, PLUS, MINUS -> {
			}
			default -> throw new ParseException("Unexpected token: '" + lexer.currentToken().termName + "'", lexer.currentPos());
		}
		return res;
	}

	public Node fTerm() throws ParseException {
		Node res = new Node("f", lexer.currentToken().termValue);
		
		switch (lexer.currentToken()) {
			case LP -> {
				Node LP = new Node("LP", lexer.currentToken().termValue);
				res.tokenChildren.add(LP);
				lexer.nextToken();
				
				Node e = eTerm();
				res.tokenChildren.add(e);
				res.value = e.value;
				Node RP = new Node("RP", lexer.currentToken().termValue);
				res.tokenChildren.add(RP);
				lexer.nextToken();
				
			}
			case MINUS -> {
				Node MINUS = new Node("MINUS", lexer.currentToken().termValue);
				res.tokenChildren.add(MINUS);
				lexer.nextToken();
				
				Node t = tTerm();
				res.tokenChildren.add(t);
				
				Node eState = eStateTerm(0 - t.value);
				res.tokenChildren.add(eState);
				res.value = eState.value;
			}
			case FLOAT -> {
				Node FLOAT = new Node("FLOAT", lexer.currentToken().termValue);
				res.tokenChildren.add(FLOAT);
				lexer.nextToken();
				res.value = Double.valueOf(FLOAT.tokenValue);
			}
			case INT -> {
				Node INT = new Node("INT", lexer.currentToken().termValue);
				res.tokenChildren.add(INT);
				lexer.nextToken();
				res.value = Double.valueOf(INT.tokenValue);
			}
			default -> throw new ParseException("Unexpected token: '" + lexer.currentToken().termName + "'", lexer.currentPos());
		}
		return res;
	}

}