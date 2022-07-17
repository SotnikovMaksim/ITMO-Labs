package expression.generic;


import java.math.BigInteger;
import expression.exceptions.*;

public class Main {
    private static String expression;

    public static <T> void main(String[] args) {
        expression = args[1];
        if (args[0].equals("-i")) {
            GenericADT<Integer> gen = new GenericInt();
            ExpressionParser<Integer> parser = new ExpressionParser<>(gen);
            resolveExpression(parser.parse(expression), gen);
        } else if (args[0].equals("-d")) {
            GenericADT<Double> gen = new GenericDouble();
            ExpressionParser<Double> parser = new ExpressionParser<>(gen);
            resolveExpression(parser.parse(expression), gen);
        } else if (args[0].equals("-bi")) {
            GenericADT<BigInteger> gen = new GenericBigInt();
            ExpressionParser<BigInteger> parser = new ExpressionParser<>(gen);
            resolveExpression(parser.parse(expression), gen);
        } else {
            throw new UnexpectedType("This type does not support");
        }
    }

    private static  <T> void resolveExpression(TripleGeneric<T> e, GenericADT<T> gen) {
        for (int i = -2; i < 3; i++) {
            for (int j = -2; j < 2; j++) {
                for (int k = -2; k < 3; k++) {
                    try {
                        T res = e.evaluate(gen.parse(i), gen.parse(j), gen.parse(k));
                        System.out.println(String.format("\nx = %d. y = %d. z = %d.\n", i, j, k) + expression + " = " + res);
                    } catch (RuntimeException exc) {
                        System.out.println(exc + String.format("\nx = %d. y = %d. z = %d.", i, j, k));
                    }
                }
            }
        }
    }
}