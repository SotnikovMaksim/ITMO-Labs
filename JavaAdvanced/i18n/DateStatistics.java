package info.kgeorgiy.ja.sotnikov.i18n;

import java.text.BreakIterator;
import java.text.DateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;

public class DateStatistics extends AbstractStatistics {

    public DateStatistics(String text, Locale textLocale) {
        super(text, textLocale);
    }

    public List<Date> collectStatistics() {
        DateFormat[] dateFormats = {
                DateFormat.getDateInstance(DateFormat.DEFAULT, textLocale),
                DateFormat.getDateInstance(DateFormat.SHORT, textLocale),
                DateFormat.getDateInstance(DateFormat.MEDIUM, textLocale),
                DateFormat.getDateInstance(DateFormat.LONG, textLocale),
                DateFormat.getDateInstance(DateFormat.FULL, textLocale),
        };

        return collectNumericStatistics(BreakIterator.getWordInstance(textLocale),
                (position) -> {
                    for (final DateFormat format: dateFormats) {
                        Date date = format.parse(text, position);

                        if (date != null) {
                            return date;
                        }
                    }

                    return null;
                }
        );
    }
}
