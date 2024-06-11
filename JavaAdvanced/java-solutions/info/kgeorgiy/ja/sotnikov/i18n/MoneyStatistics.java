package info.kgeorgiy.ja.sotnikov.i18n;

import java.text.BreakIterator;
import java.text.NumberFormat;
import java.util.List;
import java.util.Locale;

public class MoneyStatistics extends AbstractStatistics {

    public MoneyStatistics(String text, Locale textLocale) {
        super(text, textLocale);
    }

    public List<Number> collectStatistics() {
        NumberFormat formatter = NumberFormat.getCurrencyInstance(textLocale);

        return collectNumericStatistics(BreakIterator.getWordInstance(textLocale),
                (position) -> formatter.parse(text, position)
        );
    }
}
