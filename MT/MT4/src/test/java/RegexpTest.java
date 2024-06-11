import exception.GrammarException;
import generated.regexp.RegexpLexer;
import generated.regexp.RegexpParser;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.text.ParseException;
import java.util.List;

public class RegexpTest extends AbstractTest<String> {

    private static final String PARSER_NAME = "Regexp";

    private static final List<String> VALID_REGEX_TESTSUITE = List.of(
            "a",                    // Простой символ
            "ab",                   // Конкатенация
            "a|b",                  // Выбор
            "a*",                   // Замыкание Клини
            "(a)",                  // Скобки, изменяющие приоритет
            "(a|b)*",               // Комбинация замыкания Клини и выбора
            "(ab)*",                // Комбинация замыкания Клини и конкатенации
            "(a|b)c",               // Комбинация выбора и конкатенации
            "a(b|c)",               // Конкатенация с выбором в скобках
            "a(b|c)*",              // Конкатенация с замыканием Клини в скобках
            "(a|b)*c",              // Замыкание Клини и конкатенация
            "a|b|c",                // Множественный выбор
            "(a|bc)*",              // Комбинация выбора и конкатенации в замыкании Клини
            "abc|a*b",              // Конкатенация и выбор с замыканием Клини
            "(a|b)*(c|d)",          // Комбинация выбора и конкатенации с изменением приоритета
            "((abc)*|a)*",          // Вложенные скобки с замыканием Клини
            "((a|b)c)*",            // Вложенное выражение с выбором и конкатенацией
            "(a|(b|c)*)*",          // Вложенный выбор и замыкание Клини
            "((a*b|ac)d|(e|f))"     // Сложное выражение с вложенностью и разными операциями
    );

    private static final List<String> INVALID_REGEX_TESTSUITE = List.of(
            "a|",         // Неполный оператор выбора
            "*",                     // Замыкание Клини без символа или подвыражения перед ним
            "a**",
            "a*****",
            "(a|b)*****",
            "(*a)",                  // Неверное использование замыкания Клини
            "|b",                    // Выражение начинается с оператора выбора
            "ab|",                   // Выражение заканчивается оператором выбора
            "(a|*b)",                // Замыкание Клини перед символом без предшествующего выражения
            "(|a|b)",                // Пустой операнд для оператора выбора в скобках
            "(a|b|)",                // Пустой операнд для оператора выбора в скобках в конце
            "(a||b)",                // Два оператора выбора подряд
            "a(b|)c",                // Пустой операнд для оператора выбора
            "a(b)c|",                // Выражение заканчивается оператором выбора
            "(a|b)*(|c|d)"           // Пустой операнд для оператора выбора в середине выражения
    );


    @BeforeAll
    public static void init() throws IOException, GrammarException {
        generateParser(PARSER_NAME);
    }

    @Test
    public void testValidRegexp() {
        for (String test : VALID_REGEX_TESTSUITE) {
            Assertions.assertDoesNotThrow(() -> abstractTest(test, ""));
        }
    }

    @Test
    public void testInvalidRegexp() {
        for (String test : INVALID_REGEX_TESTSUITE) {
            try {
                Assertions.assertThrows(ParseException.class, () -> abstractTest(test, ""));
            } catch (AssertionError e) {
                System.err.println("Test failed for input: " + test);
                throw e;
            }
        }
    }

    @Override
    protected void abstractTest(String testLine, String expected) throws ParseException {
        RegexpLexer lexer = new RegexpLexer(testLine);
        Assertions.assertEquals(expected, new RegexpParser(lexer).eTerm().value);
    }
}
