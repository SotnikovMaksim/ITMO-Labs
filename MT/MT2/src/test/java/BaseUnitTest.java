import exception.ParseException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.List;

public class BaseUnitTest {

    private static final List<String> VALID_REGEX_TESTSUITE = List.of(
            "a",                    // Простой символ
            "ab",                   // Конкатенация
            "a|b",                  // Выбор
            "a*",                   // Замыкание Клини
            "a**",
            "a*****",
            "(a|b)*****",
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
            "a(b",                   // Незакрытые скобки
            "a|",                    // Неполный оператор выбора
            "*",                     // Замыкание Клини без символа или подвыражения перед ним
            "(*a)",                  // Неверное использование замыкания Клини
            "(a|b",                  // Одна открывающая скобка без закрытия
            "|b",                    // Выражение начинается с оператора выбора
            "ab|",                   // Выражение заканчивается оператором выбора
            "(a|*b)",                // Замыкание Клини перед символом без предшествующего выражения
            "a(b|c",                 // Незакрытая скобка после оператора выбора
            "(a|b))",                // Лишняя закрывающая скобка
            "a(b|c)d)",              // Лишняя закрывающая скобка в конце
            "(a|b)*(c|d",            // Незакрытые скобки внутри выражения
            "((a)",                  // Несоответствие количества открывающих и закрывающих скобок
            "(|a|b)",                // Пустой операнд для оператора выбора в скобках
            "(a|b|)",                // Пустой операнд для оператора выбора в скобках в конце
            "(a||b)",                // Два оператора выбора подряд
            "a(b|)c",                // Пустой операнд для оператора выбора
            "a(b)c|",                // Выражение заканчивается оператором выбора
            "(a|b)*(|c|d)"           // Пустой операнд для оператора выбора в середине выражения
    );

    @Test
    public void testBaseNonThrowingExpressions() {
        final Parser parser = new Parser();

        for (String test : VALID_REGEX_TESTSUITE) {
            Assertions.assertDoesNotThrow(() -> {
                final InputStream is = new ByteArrayInputStream(test.getBytes());
                final Tree tree = parser.parse(is);

                StringBuilder sb = new StringBuilder();
                Tree.toRegExp(tree, sb);

                String regExp = sb.toString()
                        .replace("_", "")
                        .replace("$", "");

                System.out.println(regExp);

                Assertions.assertEquals(test, regExp, "On test: " + test);
            }, "On test: " + test);
        }
    }

    @Test
    public void testBaseThrowingExpressions() {
        final Parser parser = new Parser();

        for (String test : INVALID_REGEX_TESTSUITE) {
            final InputStream is = new ByteArrayInputStream(test.getBytes());

            Assertions.assertThrows(ParseException.class, () -> parser.parse(is), "On test: " + test);
        }
    }
}
