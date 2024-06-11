package generated.regexp;


import java.text.ParseException;

public class RegexpParser {

    private final RegexpLexer lexer;

    public RegexpParser(RegexpLexer lexer) {
        this.lexer = lexer;
    }

	public Node eTerm() throws ParseException {
		Node res = new Node("e", lexer.currentToken().termValue);
		
		switch (lexer.currentToken()) {
			case LP -> {
				Node p = pTerm();
				res.tokenChildren.add(p);
				
				Node e = eTerm();
				res.tokenChildren.add(e);
				
			}
			case VAR -> {
				Node p = pTerm();
				res.tokenChildren.add(p);
				
				Node e = eTerm();
				res.tokenChildren.add(e);
				
			}
			case END -> {
			}
			case RP -> {
			}
			default -> throw new ParseException("Incorrect token: '" + lexer.currentToken().termName + "'", lexer.currentPos());
		}
		return res;
	}

	public Node pTerm() throws ParseException {
		Node res = new Node("p", lexer.currentToken().termValue);
		
		switch (lexer.currentToken()) {
			case LP -> {
				Node a = aTerm();
				res.tokenChildren.add(a);
				
				Node pState = pStateTerm();
				res.tokenChildren.add(pState);
				
			}
			case VAR -> {
				Node a = aTerm();
				res.tokenChildren.add(a);
				
				Node pState = pStateTerm();
				res.tokenChildren.add(pState);
				
			}
			default -> throw new ParseException("Incorrect token: '" + lexer.currentToken().termName + "'", lexer.currentPos());
		}
		return res;
	}

	public Node pStateTerm() throws ParseException {
		Node res = new Node("pState", lexer.currentToken().termValue);
		
		switch (lexer.currentToken()) {
			case PIPE -> {
				Node PIPE = new Node("PIPE", lexer.currentToken().termValue);
				res.tokenChildren.add(PIPE);
				lexer.nextToken();
				
				Node p = pTerm();
				res.tokenChildren.add(p);
				
			}
			case LP -> {
			}
			case END -> {
			}
			case VAR -> {
			}
			case RP -> {
			}
			default -> throw new ParseException("Incorrect token: '" + lexer.currentToken().termName + "'", lexer.currentPos());
		}
		return res;
	}

	public Node aTerm() throws ParseException {
		Node res = new Node("a", lexer.currentToken().termValue);
		
		switch (lexer.currentToken()) {
			case LP -> {
				Node s = sTerm();
				res.tokenChildren.add(s);
				
				Node aState = aStateTerm();
				res.tokenChildren.add(aState);
				
			}
			case VAR -> {
				Node s = sTerm();
				res.tokenChildren.add(s);
				
				Node aState = aStateTerm();
				res.tokenChildren.add(aState);
				
			}
			default -> throw new ParseException("Incorrect token: '" + lexer.currentToken().termName + "'", lexer.currentPos());
		}
		return res;
	}

	public Node aStateTerm() throws ParseException {
		Node res = new Node("aState", lexer.currentToken().termValue);
		
		switch (lexer.currentToken()) {
			case AST -> {
				Node AST = new Node("AST", lexer.currentToken().termValue);
				res.tokenChildren.add(AST);
				lexer.nextToken();
				
			}
			case LP -> {
			}
			case END -> {
			}
			case VAR -> {
			}
			case PIPE -> {
			}
			case RP -> {
			}
			default -> throw new ParseException("Incorrect token: '" + lexer.currentToken().termName + "'", lexer.currentPos());
		}
		return res;
	}

	public Node sTerm() throws ParseException {
		Node res = new Node("s", lexer.currentToken().termValue);
		
		switch (lexer.currentToken()) {
			case LP -> {
				Node LP = new Node("LP", lexer.currentToken().termValue);
				res.tokenChildren.add(LP);
				lexer.nextToken();
				
				Node e = eTerm();
				res.tokenChildren.add(e);
				
				Node RP = new Node("RP", lexer.currentToken().termValue);
				res.tokenChildren.add(RP);
				lexer.nextToken();
				
			}
			case VAR -> {
				Node VAR = new Node("VAR", lexer.currentToken().termValue);
				res.tokenChildren.add(VAR);
				lexer.nextToken();
				
			}
			default -> throw new ParseException("Incorrect token: '" + lexer.currentToken().termName + "'", lexer.currentPos());
		}
		return res;
	}

}