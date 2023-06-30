package info.kgeorgiy.ja.sotnikov.i18n;

import java.text.BreakIterator;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;

public class NumberStatistics extends AbstractStatistics {

    public NumberStatistics(String text, Locale textLocale) {
        super(text, textLocale);
    }

    public List<Number> collectStatistics() {
        NumberFormat numberFormatter = NumberFormat.getNumberInstance(textLocale);
        NumberFormat moneyFormatter = NumberFormat.getCurrencyInstance(textLocale);
        DateFormat dateFormatter = DateFormat.getDateInstance(DateFormat.DEFAULT, textLocale);

        return collectNumericStatistics(BreakIterator.getWordInstance(textLocale),
                (position) -> {
                    Number money = moneyFormatter.parse(text, position);
                    Date date = dateFormatter.parse(text, position);
                    Number number = numberFormatter.parse(text, position);

                    if (money == null && date == null && number != null) {
                        return number;
                    }

                    return null;
                }
        );
    }
}
