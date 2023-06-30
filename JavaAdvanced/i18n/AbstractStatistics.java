package info.kgeorgiy.ja.sotnikov.i18n;

import java.text.BreakIterator;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

public abstract class AbstractStatistics {

    protected Locale textLocale;
    protected String text;

    public AbstractStatistics(String text, Locale textLocale) {
        this.textLocale = textLocale;
        this.text = text;
    }

    protected <E> List<E> collectNumericStatistics(BreakIterator boundary,
                                                           Function<ParsePosition, E> parser
    ) {
        ParsePosition position = new ParsePosition(0);
        int start = boundary.first();
        List<E> parsedObjects = new ArrayList<>();
        boundary.setText(text);

        int previous = 0;
        for (int end = boundary.next();
             end != BreakIterator.DONE;
             start = end, end = boundary.next()
        ) {
            if (start >= previous) {
                position.setIndex(start);
                E parsedObject = parser.apply(position);

                if (parsedObject != null) {
                    parsedObjects.add(parsedObject);
                    previous = position.getIndex();
                }
            }
        }

        return parsedObjects;
    }

    protected <E> List<E> collectStringStatistics(final BreakIterator boundary,
                                                         BiFunction<Integer, Integer, E> getter,
                                                         Predicate<E> filterPredicate
    ) {
        return split(boundary, getter).stream()
                .filter(filterPredicate)
                .toList();
    }

    private <E> List<E> split(final BreakIterator boundary,
                                     final BiFunction<Integer, Integer, E> getter
    ) {
        final List<E> parts = new ArrayList<>();
        boundary.setText(text);

        for (
                int begin = boundary.first(), end = boundary.next();
                end != BreakIterator.DONE;
                begin = end, end = boundary.next()
        ) {
            parts.add(getter.apply(begin, end));
        }

        return parts;
    }
}
