package info.kgeorgiy.ja.sotnikov.i18n;

import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Locale;
import java.util.Map;

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class Tests {

    private static final Path testFile = Path.of("test.txt");
    private static final Path answerFile = Path.of("answer.txt");
    private static final String SOLVE_NAME = "info.kgeorgiy.ja.sotnikov.i18n.TextStatistics";

    private static final Locale OUTPUT_LOCALE = Locale.forLanguageTag("en-US");

    private final static String ANSWER_TEMPLATE =
            """
            Analyzed file "answer.txt"
            Summary statistics
            \tNumber of sentences: %s.
            \tNumber of words: %s.
            \tNumber of numbers: %s.
            \tNumber of amounts: %s.
            \tNumber of dates: %s.
            Statistics by sentences
            \tCount of sentences: %s (%s different).
            \tMinimum sentence: "%s".
            \tMaximum sentence: "%s".
            \tMinimum sentence length: %s ("%s").
            \tMaximum sentence length: %s ("%s").
            \tAverage sentence length: %s.
            Statistics by words
            \tCount of words: %s (%s different).
            \tMinimum word: "%s".
            \tMaximum word: "%s".
            \tMinimum word length: %s ("%s").
            \tMaximum word length: %s ("%s").
            \tAverage word length: %s.
            Statistics by numbers
            \tCount of numbers: %s (%s different).
            \tMinimum number: %s.
            \tMaximum number: %s.
            \tAverage number: %s.
            Statistics on amounts of money
            \tNumber of sums: %s (%s different).
            \tMinimum amount: %s.
            \tMaximum amount: %s.
            \tAverage amount: %s.
            Statistics by dates
            \tNumber of dates: %s (%s different).
            \tMinimum date: %s.
            \tMaximum date: %s.
            \tAverage date: %s.
            """;

    private static final Map<String, String[]> ENGLISH = Map.of(
            "The company made a profit of $100,000 on May 15, 2023.",
            new String[]{
                    "1",
                    "8",
                    "2",
                    "1",
                    "1",
                    "1", "1",
                    "The company made a profit of $100,000 on May 15, 2023.",
                    "The company made a profit of $100,000 on May 15, 2023.",
                    "54", "The company made a profit of $100,000 on May 15, 2023.",
                    "54", "The company made a profit of $100,000 on May 15, 2023.",
                    "54.0",
                    "8", "8",
                    "a",
                    "The",
                    "1", "a",
                    "7", "company",
                    "3.5",
                    "2", "2",
                    "15",
                    "2,023",
                    "1,019",
                    "1", "1",
                    "$100,000.00",
                    "$100,000.00",
                    "$100,000.00",
                    "1", "1",
                    "Monday, May 15, 2023",
                    "Monday, May 15, 2023",
                    "Monday, May 15, 2023"
            }
    );

    private static final Map<String, String[]> RUSSIAN = Map.of(
            "Компания заработала 300 000 ₽ 20 мая 2023 г..",
            new String[]{
                    "1",
                    "4",
                    "1",
                    "1",
                    "1",
                    "1", "1",
                    "Компания заработала 300 000 ₽ 20 мая 2023 г..",
                    "Компания заработала 300 000 ₽ 20 мая 2023 г..",
                    "45", "Компания заработала 300 000 ₽ 20 мая 2023 г..",
                    "45", "Компания заработала 300 000 ₽ 20 мая 2023 г..",
                    "45.0",
                    "4", "4",
                    "г",
                    "мая",
                    "1", "г",
                    "10", "заработала",
                    "5.5",
                    "1", "1",
                    "2,023",
                    "2,023",
                    "2,023",
                    "1", "1",
                    "300 000,00 ₽",
                    "300 000,00 ₽",
                    "300 000,00 ₽",
                    "1", "1",
                    "Saturday, May 20, 2023",
                    "Saturday, May 20, 2023",
                    "Saturday, May 20, 2023"
            }
    );

    private static final Map<String, String[]> GERMAN = Map.of(
            "Das Unternehmen machte am 26.02.2023 einen Gewinn von 10.789,80 €. Ich bin ein dummes Schwein.",
            new String[]{
                    "2",
                    "12",
                    "0",
                    "1",
                    "1",
                    "2", "2",
                    "Das Unternehmen machte am 26.02.2023 einen Gewinn von 10.789,80 €. ",
                    "Ich bin ein dummes Schwein.",
                    "27", "Ich bin ein dummes Schwein.",
                    "67", "Das Unternehmen machte am 26.02.2023 einen Gewinn von 10.789,80 €. ",
                    "47.0",
                    "12", "12",
                    "am",
                    "von",
                    "2", "am",
                    "11", "Unternehmen",
                    "4.833333333333333",
                    "0", "0",
                    "none",
                    "none",
                    "none",
                    "1", "1",
                    "10.789,80 €",
                    "10.789,80 €",
                    "10.789,80 €",
                    "1", "1",
                    "Sunday, February 26, 2023",
                    "Sunday, February 26, 2023",
                    "Sunday, February 26, 2023"
            }
    );

    public Tests() {
    }

    @Test
    public void test01_eng() throws ClassNotFoundException, NoSuchMethodException, InvocationTargetException, InstantiationException, IllegalAccessException {
        test_language(Locale.forLanguageTag("en-US"), ENGLISH);
    }

    @Test
    public void test02_rus() throws ClassNotFoundException, NoSuchMethodException, InvocationTargetException, InstantiationException, IllegalAccessException {
        test_language(Locale.forLanguageTag("ru-RU-#Cyrl"), RUSSIAN);
    }

    @Test
    public void test03_ger() throws ClassNotFoundException, NoSuchMethodException, InvocationTargetException, InstantiationException, IllegalAccessException {
        test_language(Locale.GERMANY, GERMAN);
    }

    private void test_language(Locale test_locale, Map<String, String[]> tests) throws InvocationTargetException, InstantiationException, IllegalAccessException, ClassNotFoundException, NoSuchMethodException {
        Constructor<?> constructor = getConstructor();

        try {
            for (final Map.Entry<String, String[]> test : tests.entrySet()) {
                Files.write(testFile, test.getKey().getBytes());

                TextStatistics instance = (info.kgeorgiy.ja.sotnikov.i18n.TextStatistics) constructor.newInstance(
                        test_locale,
                        OUTPUT_LOCALE,
                        testFile,
                        answerFile
                );

                instance.collectStatistics();

                String programAnswer = Files.readString(answerFile, StandardCharsets.UTF_8);
                String expected = getAnswer(test.getValue());

                Assert.assertEquals(expected, programAnswer);
            }

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private String getAnswer(String[] answer) {
        return String.format(ANSWER_TEMPLATE, Arrays.asList(answer).toArray());
    }

    private Constructor<?> getConstructor() throws ClassNotFoundException, NoSuchMethodException {
        return Class.forName(SOLVE_NAME).getConstructor(Locale.class, Locale.class, Path.class, Path.class);
    }
}

