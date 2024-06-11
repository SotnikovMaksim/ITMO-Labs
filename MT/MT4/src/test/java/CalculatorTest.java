import exception.GrammarException;
import generated.calculator.CalculatorLexer;
import generated.calculator.CalculatorParser;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.text.ParseException;

public class CalculatorTest extends AbstractTest<Double> {

    private static final String PARSER_NAME = "Calculator";

    @BeforeAll
    public static void init() throws IOException, GrammarException {
        generateParser(PARSER_NAME);
    }

    @Test
    public void test01_singleInt() throws ParseException {
        String test = "2";
        abstractTest(test, 2.0);
    }

    @Test
    public void test02_singleNegativeInt() throws ParseException {
        String test = "0-5";
        abstractTest(test, -5.0);
    }

    @Test
    public void test04_addition() throws ParseException {
        String test = "2 + 3";
        abstractTest(test, 5.0);
    }

    @Test
    public void test05_subtraction() throws ParseException {
        String test = "7 - 4";
        abstractTest(test, 3.0);
    }

    @Test
    public void test06_multiplication() throws ParseException {
        String test = "4 * 5";
        abstractTest(test, 20.0);
    }

    @Test
    public void test07_division() throws ParseException {
        String test = "10 / 2";
        abstractTest(test, 5.0);
    }

    @Test
    public void test08_mixedOperations() throws ParseException {
        String test = "2 + 3 * 4 - 1";
        abstractTest(test, 13.0);
    }

    @Test
    public void test09_parentheses() throws ParseException {
        String test = "(3 + 2) * 4";
        abstractTest(test, 20.0);
    }

    @Test
    public void test10_complexExpression() throws ParseException {
        String test = "2 * (5 - 3) + 8 / 2";
        abstractTest(test, 8.0);
    }

    @Test
    public void test12_zeroDivision() throws ParseException {
        String test = "5 / 0";
        abstractTest(test, Double.POSITIVE_INFINITY);
    }

    @Test
    public void test14_whitespaceInExpression() throws ParseException {
        String test = "  3   +   7   ";
        abstractTest(test, 10.0);
    }

    @Test
    public void test16_largeExpression() throws ParseException {
        String test = "10 + 5 * (20 - 3) / 2";
        abstractTest(test, 52.5);
    }

    @Override
    protected void abstractTest(String testLine, Double expected) throws ParseException {
        CalculatorLexer lexer = new CalculatorLexer(testLine);
        Assertions.assertEquals(expected, new CalculatorParser(lexer).eTerm().value);
    }
}
