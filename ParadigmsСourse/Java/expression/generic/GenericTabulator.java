package expression.generic;

import expression.exceptions.*;
import java.math.BigInteger;

public class GenericTabulator implements Tabulator {
    // :NOTE: лушче сделать x2, y2, z2, чтобы потом не писать this
    private int x1, x;
    private int y1, y;
    private int z1, z;

    @Override
    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) {
        return tab(mode, expression, x1, x2, y1, y2, z1, z2);
    }

    private <T> Object[][][] resolveExpression(Object[][][] res, TripleGeneric<T> e, GenericADT<T> gen) {
        for (int i = 0; i < x; i++) {
            for (int j = 0; j < y; j++) {
                for (int k = 0; k < z; k++) {
                    try {
                        res[i][j][k] = e.evaluate(gen.parse(x1 + i), gen.parse(y1 + j), gen.parse(z1 + k));
                        // :NOTE: RuntimeException - не ловят, нужен специализированный
                    } catch (RuntimeException exc) {
                        res[i][j][k] = null;
                    }
                }
            }
        }
        return res;
    }

    private <T> Object[][][] tab(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) {
        this.x1 = x1;
        this.x = x2 - x1 + 1;
        this.y1 = y1;
        this.y = y2 - y1 + 1;
        this.z1 = z1;
        this.z = z2 - z1 + 1;
        Object[][][] res = new Object[x][y][z];
        // :NOTE: смотрим подсказки идеи
        if (mode.equals("i")) {
            GenericADT<Integer> gen = new GenericInt();
            ExpressionParser<Integer> parser = new ExpressionParser<>(gen);
            return resolveExpression(res, parser.parse(expression), gen);
        } else if (mode.equals("d")) {
            GenericADT<Double> gen = new GenericDouble();
            ExpressionParser<Double> parser = new ExpressionParser<>(gen);
            return resolveExpression(res, parser.parse(expression), gen);
        } else if (mode.equals("bi")) {
            GenericADT<BigInteger> gen = new GenericBigInt();
            ExpressionParser<BigInteger> parser = new ExpressionParser<>(gen);
            return resolveExpression(res, parser.parse(expression), gen);
        }
        throw new UnexpectedType("This type does not support");
    }
}