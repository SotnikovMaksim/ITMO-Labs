import error.ParseError;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.misc.Pair;
import org.antlr.v4.runtime.tree.ParseTree;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static utils.SymbolsUtils.NEW_LINE;
import static utils.SymbolsUtils.TAB;

public class FuncLangToJavaVisitor extends FuncLangBaseVisitor<String> {

    private static final Set<String> UNARY_ARITHMETIC_OPERATORS = Set.of(
            "neg"
    );

    private static final Set<String> UNARY_BOOLEAN_OPERATORS = Set.of(
            "not"
    );

    private static final Set<String> BINARY_ARITHMETIC_OPERATORS = Set.of(
            "+", "-", "*", "/", "%"
    );

    private static final Set<String> BINARY_COMPARISON_OPERATORS = Set.of(
            "<", "<=", "==", ">=", ">"
    );

    private static final Set<String> COMPARABLE_TYPES = Set.of(
            "Integer", "Float", "None"
    );

    private static final Set<String> NON_SUITABLE_FOR_INVERSION = Set.of(
            "Integer", "Float", "String"
    );

    private static final Set<String> NON_SUITABLE_FOR_ADDITION = Set.of(

    );

    private static final Set<String> NON_SUITABLE_FOR_NEGATE = Set.of(
            "String"
    );

    private static final Set<String> INTEGER_OPERATORS = Set.of(
            "%"
    );

    private static final Set<OperandType> LITERALS = Set.of(
            OperandType.INT, OperandType.FLOAT, OperandType.STRING
    );

    private final List<String> mainFunctionContent = new ArrayList<>();

    @Override
    public String visitProgram(FuncLangParser.ProgramContext ctx) {
        String result = ctx.children
                .stream()
                .map(this::visit)
                .map(this::createClassBody)
                .collect(Collectors.joining(NEW_LINE));

        if (!mainFunctionContent.isEmpty()) {
            result += createClassBody(createMainFunction());
        }

        return result;
    }

    @Override
    public String visitBlock(FuncLangParser.BlockContext ctx) {
        return ctx.children
                .stream()
                .map(this::visit)
                .collect(Collectors.joining(NEW_LINE));
    }

    @Override
    public String visitFunctionLikeExpr(FuncLangParser.FunctionLikeExprContext ctx) {
        ParseTree firstChild = ctx.getChild(0);

        if (firstChild instanceof FuncLangParser.FunctionCallContext) {
            return visitFunctionCall((FuncLangParser.FunctionCallContext) firstChild);
        } else if (firstChild instanceof FuncLangParser.IfExprContext) {
            return visitIfExpr((FuncLangParser.IfExprContext) firstChild);
        } else if (firstChild instanceof FuncLangParser.VarDefContext) {
            return visitVarDef((FuncLangParser.VarDefContext) firstChild);
        } else if (firstChild instanceof FuncLangParser.PrintContext) {
            return visitPrint((FuncLangParser.PrintContext) firstChild);
        } else if (firstChild instanceof FuncLangParser.ReturnContext) {
            return visitReturn((FuncLangParser.ReturnContext) firstChild);
        }
        throw new RuntimeException("Unknown function-like expression: " + firstChild.getText());
    }

    @Override
    public String visitFunctionDef(FuncLangParser.FunctionDefContext ctx) {
        String functionName = ctx.IDENTIFIER().get(0).getText();
        String params = ctx.IDENTIFIER()
                .stream()
                .skip(1)
                .map(x -> "Object " + x.getText())
                .collect(Collectors.joining(", "));
        String body = visit(ctx.block());

        String result = functionName + "(" + params + ") {" + NEW_LINE +
                createFunctionBody(body) + NEW_LINE +
                "}" + NEW_LINE;

        if (result.contains("return")) {
            result = "public static Object " + result;
        } else {
            result = "public static void " + result;
        }

        return result;
    }

    @Override
    public String visitReturn(FuncLangParser.ReturnContext ctx) {
        String returnValue = visit(ctx.expr());

        return "return " + returnValue;
    }

    @Override
    public String visitFunctionCall(FuncLangParser.FunctionCallContext ctx) {
        String functionName = ctx.IDENTIFIER().getText();
        String args = ctx.expr()
                .stream()
                .map(this::visit)
                .collect(Collectors.joining(", "));

        return functionName + "(" + args + ")";
    }

    @Override
    public String visitVarDef(FuncLangParser.VarDefContext ctx) {
        String varName = ctx.IDENTIFIER().getText();
        String varValue = visit(ctx.expr());

        return "var " + varName + " = " + varValue + ";" + NEW_LINE;
    }

    @Override
    public String visitOperation(FuncLangParser.OperationContext ctx) {
        String result;

        if (ctx.unaryOp() != null) {
            result = handleUnaryOperation(ctx);
        } else if (ctx.binOp() != null) {
            result = handleBinaryOperation(ctx);
        } else {
            throw new ParseError("Unknown operator type");
        }

        return result;
    }

    private String handleBinaryOperation(FuncLangParser.OperationContext ctx) {
        String result;

        String op = ctx.binOp().getText();
        OperandType leftOperandType = getOperandType(ctx.expr(0));
        OperandType rightOperandType = getOperandType(ctx.expr(1));
        String leftOperand = visit(ctx.expr(0));
        String rightOperand = visit(ctx.expr(1));

        if (BINARY_COMPARISON_OPERATORS.contains(op)) {
            if (!COMPARABLE_TYPES.contains(leftOperandType.value) ||
                    !COMPARABLE_TYPES.contains(rightOperandType.value)
            ) {
                throw new ParseError("Unsupported types for compare operation: " + leftOperandType + ", " + rightOperandType);
            }

            Pair<OperandType, OperandType> typePair = resolveOperands(leftOperandType, rightOperandType);

            leftOperandType = typePair.a;
            rightOperandType = typePair.b;

            result = "(" +
                    unwrapOperandType(leftOperandType) + leftOperand +
                    " " + op + " " +
                    unwrapOperandType(rightOperandType) + rightOperand +
                    ")";
        } else if (BINARY_ARITHMETIC_OPERATORS.contains(op)) {
            if ("+".equals(op) &&
                    (NON_SUITABLE_FOR_ADDITION.contains(leftOperandType.value) ||
                            NON_SUITABLE_FOR_ADDITION.contains(rightOperandType.value))
            ) {
                throw new ParseError("String types aren't suitable for non-addition arithmetic operations");
            }

            Pair<OperandType, OperandType> typePair = resolveOperands(leftOperandType, rightOperandType);

            leftOperandType = typePair.a;
            rightOperandType = typePair.b;

            if (INTEGER_OPERATORS.contains(op) && leftOperandType == OperandType.VARIABLE) {
                leftOperandType = OperandType.INT;
            }

            if (INTEGER_OPERATORS.contains(op) && rightOperandType == OperandType.VARIABLE) {
                rightOperandType = OperandType.INT;
            }

            result = "(" +
                    unwrapOperandType(leftOperandType) + leftOperand +
                    " " + op + " " +
                    unwrapOperandType(rightOperandType) + rightOperand +
                    ")";
        } else {
            throw new ParseError("Unknown binary operation type: " + op);
        }

        return result;
    }

    private Pair<OperandType, OperandType> resolveOperands(OperandType leftOperandType, OperandType rightOperandType) {
        if (leftOperandType == OperandType.VARIABLE && (!"None".equals(rightOperandType.value))) {
            leftOperandType = rightOperandType;
            rightOperandType = OperandType.UNNECESSARY;
        } else if (rightOperandType == OperandType.VARIABLE && (!"None".equals(leftOperandType.value))) {
            rightOperandType = leftOperandType;
            leftOperandType = OperandType.UNNECESSARY;
        } else {
            if (!"None".equals(leftOperandType.value)) {
                leftOperandType = OperandType.UNNECESSARY;
            }
            if (!"None".equals(rightOperandType.value)) {
                rightOperandType = OperandType.UNNECESSARY;
            }
        }

        return new Pair<>(leftOperandType, rightOperandType);
    }

    private String handleUnaryOperation(FuncLangParser.OperationContext ctx) {
        String result;

        String op = ctx.unaryOp().getText();
        OperandType operandType = getOperandType(ctx.expr(0));
        String operand = visit(ctx.expr(0));

        if (UNARY_BOOLEAN_OPERATORS.contains(op)) {
            if ("not".equals(op) &&
                    NON_SUITABLE_FOR_INVERSION.contains(operandType.value)
            ) {
                throw new ParseError("Unsupported type for 'not': " + operandType);
            }

            if (LITERALS.contains(operandType)) {
                operandType = OperandType.UNNECESSARY;
            }

            result = "!" + "(" +
                    unwrapOperandType(operandType) + operand +
                    ")";
        } else if (UNARY_ARITHMETIC_OPERATORS.contains(op)) {
            if ("neg".equals(op) &&
                    NON_SUITABLE_FOR_NEGATE.contains(operandType.value)
            ) {
                throw new ParseError("Non suitable for 'negate' operand type: " + operandType);
            }

            if (LITERALS.contains(operandType)) {
                operandType = OperandType.UNNECESSARY;
            }

            result = "-" + "(" +
                    unwrapOperandType(operandType) + operand +
                    ")";
        } else {
            throw new ParseError("Unknown unary operation type: " + op);
        }

        return result;
    }

    private String unwrapOperandType(OperandType type) {
        return "None".equals(type.value) || type.value.isEmpty() ?
                "" : "(" + type.value + ") ";
    }

    private OperandType getOperandType(FuncLangParser.ExprContext operand) {
        OperandType result;

        if (operand.literal() != null) {
            result = operandTypeFromLiteral(operand.literal());
        } else if (operand.operation() != null) {
            if (isBooleanOperation(operand.operation())) {
                result = OperandType.BOOLEAN_FUNC;
            } else if (isArithmeticOperation(operand.operation())) {
                result = OperandType.ARITHMETIC_FUNC;
            } else {
                throw new ParseError("Unknown operator type");
            }
        } else if (operand.IDENTIFIER() != null) {
            result = OperandType.VARIABLE;
        } else if (operand.functionLikeExpr() != null &&
                operand.functionLikeExpr().functionCall() != null
        ) {
            result = OperandType.FUNCTION_CALL;
        } else {
            throw new ParseError("None value and non function calls are not supported in operations");
        }

        return result;
    }

    private OperandType operandTypeFromLiteral(FuncLangParser.LiteralContext literal) {
        OperandType result;

        if (literal.INT() != null) {
            result = OperandType.INT;
        } else if (literal.FLOAT() != null) {
            result = OperandType.FLOAT;
        } else if (literal.STRING() != null) {
            result = OperandType.STRING;
        } else {
            throw new ParseError("Unknown literal type");
        }

        return result;
    }

    private boolean isBooleanOperation(FuncLangParser.OperationContext operation) {
        return (operation.unaryOp() != null && operation.unaryOp().boolUnaruOp() != null) ||
                (operation.binOp() != null && operation.binOp().boolBinOp() != null);
    }

    private boolean isArithmeticOperation(FuncLangParser.OperationContext operation) {
        return (operation.unaryOp() != null && operation.unaryOp().arithUnaryOp() != null) ||
                (operation.binOp() != null && operation.binOp().arithBinOp() != null);
    }

    @Override
    public String visitIfExpr(FuncLangParser.IfExprContext ctx) {
        String condition = visit(ctx.expr(0));
        String thenBranch = addEndOfLine(visit(ctx.expr(1)));
        String elseBranch = ctx.expr().size() > 2 ? addEndOfLine(visit(ctx.expr(2))) : null;

        return "if (" + condition + ") {" + NEW_LINE +
                TAB + thenBranch + NEW_LINE +
                "}" +
                (elseBranch != null ?
                        " else {" + NEW_LINE +
                                TAB + elseBranch + NEW_LINE +
                                "}" : "") + NEW_LINE;
    }

    private String addEndOfLine(String lines) {
        return Arrays.stream(lines.split(NEW_LINE)).map(s -> s + ";").collect(Collectors.joining(NEW_LINE));
    }

    @Override
    public String visitPrint(FuncLangParser.PrintContext ctx) {
        String contentToPrint = visit(ctx.expr());

        return "System.out.println(" + contentToPrint + ")";
    }

    @Override
    public String visitList(FuncLangParser.ListContext ctx) {
        String elements = ctx.expr()
                .stream()
                .map(this::visit)
                .collect(Collectors.joining(", "));
        return "java.util.Arrays.asList(" + elements + ")";
    }

    @Override
    public String visitLiteral(FuncLangParser.LiteralContext ctx) {
        return visitIfPresent(ctx, "Unknown literal type: " + ctx);
    }

    @Override
    public String visitExpr(FuncLangParser.ExprContext ctx) {
        String result;

        if (ctx.IDENTIFIER() != null) {
            result = ctx.IDENTIFIER().getText();
        } else if (ctx.literal() != null) {
            result = ctx.literal().getText();
        } else {
            result = visitIfPresent(ctx, "Unknown expression type: " + ctx);

            if (ctx.depth() <= 4
            ) {
                result += ";";
            }
        }

        if (ctx.depth() == 2 && notInner(ctx)) {
            mainFunctionContent.add(result);
            result = "";
        }

        return result;
    }

    private boolean notInner(FuncLangParser.ExprContext ctx) {
        ParserRuleContext parent = ctx.getParent().getParent();

        return parent instanceof FuncLangParser.FunctionDefContext ||
                parent instanceof FuncLangParser.IfExprContext;
    }

    private String visitIfPresent(ParserRuleContext ctx, String errorMessageIfNot) {
        if (ctx.getChildCount() == 3) {
            return visit(ctx.getChild(1));
        } else if (ctx.getChildCount() == 1) {
            return visit(ctx.getChild(0));
        } else if (ctx.getChildCount() == 0) {
            return ctx.getText();
        }
        throw new ParseError(errorMessageIfNot);
    }

    private String createClassBody(String body) {
        return createClassBody(Arrays.asList(body.split(NEW_LINE)));
    }

    private String createClassBody(List<String> body) {
        return body.stream()
                .map(s -> TAB + s)
                .collect(Collectors.joining(NEW_LINE));
    }

    private String createFunctionBody(String body) {
        return createFunctionBody(Arrays.asList(body.split(NEW_LINE)));
    }

    private String createFunctionBody(List<String> body) {
        return body.stream()
                .map(s -> TAB + s)
                .collect(Collectors.joining(NEW_LINE));
    }

    private String createMainFunction() {
        return NEW_LINE +
                "public static void main(String[] args) {" + NEW_LINE +
                createFunctionBody(mainFunctionContent) + NEW_LINE +
                "}" + NEW_LINE;
    }

    private enum OperandType {
        UNNECESSARY(""),
        INT("Integer"),
        FLOAT("Float"),
        STRING("String"),
        VARIABLE("None"),
        BOOLEAN_FUNC("None"),
        ARITHMETIC_FUNC("None"),
        FUNCTION_CALL("None");

        private final String value;

        OperandType(String value) {
            this.value = value;
        }
    }
}
