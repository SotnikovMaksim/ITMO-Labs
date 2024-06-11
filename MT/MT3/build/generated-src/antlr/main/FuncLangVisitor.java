// Generated from FuncLang.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link FuncLangParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface FuncLangVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#program}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProgram(FuncLangParser.ProgramContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#functionDef}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFunctionDef(FuncLangParser.FunctionDefContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#block}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlock(FuncLangParser.BlockContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpr(FuncLangParser.ExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#return}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReturn(FuncLangParser.ReturnContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#functionLikeExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFunctionLikeExpr(FuncLangParser.FunctionLikeExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#functionCall}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFunctionCall(FuncLangParser.FunctionCallContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#print}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrint(FuncLangParser.PrintContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#ifExpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIfExpr(FuncLangParser.IfExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#varDef}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVarDef(FuncLangParser.VarDefContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#unaryOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnaryOp(FuncLangParser.UnaryOpContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#arithUnaryOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArithUnaryOp(FuncLangParser.ArithUnaryOpContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#boolUnaruOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBoolUnaruOp(FuncLangParser.BoolUnaruOpContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#binOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBinOp(FuncLangParser.BinOpContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#arithBinOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArithBinOp(FuncLangParser.ArithBinOpContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#boolBinOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBoolBinOp(FuncLangParser.BoolBinOpContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#operation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOperation(FuncLangParser.OperationContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#list}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitList(FuncLangParser.ListContext ctx);
	/**
	 * Visit a parse tree produced by {@link FuncLangParser#literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLiteral(FuncLangParser.LiteralContext ctx);
}