// Generated from FuncLang.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link FuncLangParser}.
 */
public interface FuncLangListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#program}.
	 * @param ctx the parse tree
	 */
	void enterProgram(FuncLangParser.ProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#program}.
	 * @param ctx the parse tree
	 */
	void exitProgram(FuncLangParser.ProgramContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#functionDef}.
	 * @param ctx the parse tree
	 */
	void enterFunctionDef(FuncLangParser.FunctionDefContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#functionDef}.
	 * @param ctx the parse tree
	 */
	void exitFunctionDef(FuncLangParser.FunctionDefContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#block}.
	 * @param ctx the parse tree
	 */
	void enterBlock(FuncLangParser.BlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#block}.
	 * @param ctx the parse tree
	 */
	void exitBlock(FuncLangParser.BlockContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterExpr(FuncLangParser.ExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitExpr(FuncLangParser.ExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#return}.
	 * @param ctx the parse tree
	 */
	void enterReturn(FuncLangParser.ReturnContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#return}.
	 * @param ctx the parse tree
	 */
	void exitReturn(FuncLangParser.ReturnContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#functionLikeExpr}.
	 * @param ctx the parse tree
	 */
	void enterFunctionLikeExpr(FuncLangParser.FunctionLikeExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#functionLikeExpr}.
	 * @param ctx the parse tree
	 */
	void exitFunctionLikeExpr(FuncLangParser.FunctionLikeExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#functionCall}.
	 * @param ctx the parse tree
	 */
	void enterFunctionCall(FuncLangParser.FunctionCallContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#functionCall}.
	 * @param ctx the parse tree
	 */
	void exitFunctionCall(FuncLangParser.FunctionCallContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#print}.
	 * @param ctx the parse tree
	 */
	void enterPrint(FuncLangParser.PrintContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#print}.
	 * @param ctx the parse tree
	 */
	void exitPrint(FuncLangParser.PrintContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#ifExpr}.
	 * @param ctx the parse tree
	 */
	void enterIfExpr(FuncLangParser.IfExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#ifExpr}.
	 * @param ctx the parse tree
	 */
	void exitIfExpr(FuncLangParser.IfExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#varDef}.
	 * @param ctx the parse tree
	 */
	void enterVarDef(FuncLangParser.VarDefContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#varDef}.
	 * @param ctx the parse tree
	 */
	void exitVarDef(FuncLangParser.VarDefContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#unaryOp}.
	 * @param ctx the parse tree
	 */
	void enterUnaryOp(FuncLangParser.UnaryOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#unaryOp}.
	 * @param ctx the parse tree
	 */
	void exitUnaryOp(FuncLangParser.UnaryOpContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#arithUnaryOp}.
	 * @param ctx the parse tree
	 */
	void enterArithUnaryOp(FuncLangParser.ArithUnaryOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#arithUnaryOp}.
	 * @param ctx the parse tree
	 */
	void exitArithUnaryOp(FuncLangParser.ArithUnaryOpContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#boolUnaruOp}.
	 * @param ctx the parse tree
	 */
	void enterBoolUnaruOp(FuncLangParser.BoolUnaruOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#boolUnaruOp}.
	 * @param ctx the parse tree
	 */
	void exitBoolUnaruOp(FuncLangParser.BoolUnaruOpContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#binOp}.
	 * @param ctx the parse tree
	 */
	void enterBinOp(FuncLangParser.BinOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#binOp}.
	 * @param ctx the parse tree
	 */
	void exitBinOp(FuncLangParser.BinOpContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#arithBinOp}.
	 * @param ctx the parse tree
	 */
	void enterArithBinOp(FuncLangParser.ArithBinOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#arithBinOp}.
	 * @param ctx the parse tree
	 */
	void exitArithBinOp(FuncLangParser.ArithBinOpContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#boolBinOp}.
	 * @param ctx the parse tree
	 */
	void enterBoolBinOp(FuncLangParser.BoolBinOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#boolBinOp}.
	 * @param ctx the parse tree
	 */
	void exitBoolBinOp(FuncLangParser.BoolBinOpContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#operation}.
	 * @param ctx the parse tree
	 */
	void enterOperation(FuncLangParser.OperationContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#operation}.
	 * @param ctx the parse tree
	 */
	void exitOperation(FuncLangParser.OperationContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#list}.
	 * @param ctx the parse tree
	 */
	void enterList(FuncLangParser.ListContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#list}.
	 * @param ctx the parse tree
	 */
	void exitList(FuncLangParser.ListContext ctx);
	/**
	 * Enter a parse tree produced by {@link FuncLangParser#literal}.
	 * @param ctx the parse tree
	 */
	void enterLiteral(FuncLangParser.LiteralContext ctx);
	/**
	 * Exit a parse tree produced by {@link FuncLangParser#literal}.
	 * @param ctx the parse tree
	 */
	void exitLiteral(FuncLangParser.LiteralContext ctx);
}