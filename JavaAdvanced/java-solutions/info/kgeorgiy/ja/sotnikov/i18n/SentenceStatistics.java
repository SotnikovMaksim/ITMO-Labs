package info.kgeorgiy.ja.sotnikov.i18n;

import java.text.BreakIterator;
import java.util.List;
import java.util.Locale;
import java.util.function.BiFunction;
import java.util.function.Predicate;

public class SentenceStatistics extends AbstractStatistics{

    public SentenceStatistics(String text, Locale textLocale) {
        super(text, textLocale);
    }

    public List<String> collectStatistics() {
        Predicate<String> filterPredicate = word -> word.codePoints().anyMatch(Character::isLetter);
        BiFunction<Integer, Integer, String> getter = (begin, end) -> text.substring(begin, end);

        return collectStringStatistics(
                BreakIterator.getSentenceInstance(textLocale),
                getter,
                filterPredicate
        );
    }
}
