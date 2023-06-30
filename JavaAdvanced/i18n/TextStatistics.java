package info.kgeorgiy.ja.sotnikov.i18n;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.text.Collator;
import java.text.DateFormat;
import java.text.Format;
import java.text.NumberFormat;
import java.util.*;


public class TextStatistics {

    private static final String NEW_LINE = System.lineSeparator();
    private static final String RESOURCE_BUNDLE_PATH = "info.kgeorgiy.ja.sotnikov.i18n.MyBundle";

    private final Locale inputLocale;
    private final Locale outputLocale;

    private final Path inputFile;
    private final Path outputFile;

    private String text;

    public TextStatistics(final Locale inputLocale,
                          final Locale outputLocale,
                          final Path inputFile,
                          final Path outputFile
    ) throws IOException {
        this.inputLocale = inputLocale;
        this.outputLocale = outputLocale;
        this.inputFile = inputFile;
        this.outputFile = outputFile;

        if (Files.notExists(inputFile) || !Files.isRegularFile(inputFile)) {
            throw new IllegalArgumentException("Either input file is directory or it does not exist!");
        }

        Path parentPath = outputFile.getParent();

        if (parentPath != null && Files.notExists(outputFile.getParent())) {
            System.err.println("Parent directory of output file path does not exist!" + NEW_LINE +
                    "Trying to create parent directories...");
            Files.createDirectories(outputFile.getParent());
            System.err.println("Parent directories created successfully!");
        }
    }

    public static void main(final String[] args) {
        if (args == null || args.length != 4 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            throw new IllegalArgumentException("Invalid arguments!");
        }

        Locale inputLocale = Locale.forLanguageTag(args[0]);
        Locale outputLocale = Locale.forLanguageTag(args[1]);

        Path inputFile, outputFile;

        try {
            inputFile = Path.of(args[2]);
            outputFile = Path.of(args[3]);
        } catch (InvalidPathException e) {
            System.err.println("Invalid path!");
            e.printStackTrace();
            return;
        }

        try {
            new TextStatistics(inputLocale, outputLocale, inputFile, outputFile).collectStatistics();
        } catch (IOException e) {
            System.err.println(e.getMessage());
            e.printStackTrace();
        }
    }

    public void collectStatistics() {
        try {
            text = Files.readString(inputFile);

            Map<String, StatisticsType<?>> statistics = Map.of(
                    "money", getMoney(),
                    "sentences", getSentences(),
                    "words", getWords(),
                    "numbers", getNumbers(),
                    "dates", getDates()
            );

            try (final BufferedWriter writer = Files.newBufferedWriter(outputFile)) {
                StatisticsWriter statisticsWriter = new StatisticsWriter(writer, statistics, outputLocale);

                statisticsWriter.writeHeader();
                statisticsWriter.writeSentenceStatistic();
                statisticsWriter.writeWordStatistic();
                statisticsWriter.writeNumberStatistic();
                statisticsWriter.writeMoneyStatistic();
                statisticsWriter.writeDateStatistic();
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private StatisticsType<Number> getMoney() {
        return new NumericStatistics<>(
                new MoneyStatistics(text, inputLocale)
                        .collectStatistics()
                        .stream()
                        .map(Number::doubleValue)
                        .toList()
                ,
                Comparator.naturalOrder()
        );
    }

    private StatisticsType<String> getSentences() {
        return new StringStatistics<>(
                new SentenceStatistics(text, inputLocale).collectStatistics(),
                Collator.getInstance(inputLocale),
                String::length
        );
    }

    private StatisticsType<String> getWords() {
        return new StringStatistics<>(
                new WordStatistics(text, inputLocale).collectStatistics(),
                Collator.getInstance(inputLocale),
                String::length
        );
    }

    private StatisticsType<Number> getNumbers() {
        return new NumericStatistics<>(
                new NumberStatistics(text, inputLocale)
                        .collectStatistics()
                        .stream()
                        .map(Number::doubleValue)
                        .toList()
                ,
                Comparator.naturalOrder()
        );
    }

    private StatisticsType<Number> getDates() {
        return new NumericStatistics<>(
                new DateStatistics(text, inputLocale)
                        .collectStatistics()
                        .stream()
                        .map(v -> (double) v.getTime())
                        .toList()
                ,
                Comparator.naturalOrder()
        );
    }

    private class StatisticsWriter {

        private enum KeyPrefix {
            COMMON("common"),
            HEADER("header"),
            SENTENCES("sentences"),
            WORDS("words"),
            NUMBERS("numbers"),
            MONEY("money"),
            DATES("dates");

            private final String keyPrefix;

            KeyPrefix(String keyPrefix) {
                this.keyPrefix = keyPrefix;
            }

            public String getKeyPrefix() {
                return this.keyPrefix;
            }
        }

        private final Map<String, StatisticsType<?>> statistics;
        private final ResourceBundle bundle;
        private final BufferedWriter writer;

        private StatisticsWriter(final BufferedWriter writer,
                                 final Map<String, StatisticsType<?>> statistics,
                                 final Locale locale
        ) {
            this.bundle = ResourceBundle.getBundle(RESOURCE_BUNDLE_PATH, locale);
            this.statistics = statistics;
            this.writer = writer;
        }

        private void writeHeader() throws IOException {
            writer.write("%s \"%s\"".formatted(
                    bundle.getString(getKey(KeyPrefix.HEADER, "intro")),
                    outputFile.getFileName()));
            writer.newLine();

            writeHeaderFor(KeyPrefix.HEADER);

            KeyPrefix[] values = KeyPrefix.values();
            for (int i = 2; i < values.length; i++) {
                StatisticsType<?> stat = statistics.get(values[i].getKeyPrefix());

                writer.write("\t%s: %d.".formatted(
                        bundle.getString(getKey(KeyPrefix.HEADER, values[i].getKeyPrefix())),
                        stat.size()
                ));
                writer.newLine();
            }
        }

        private void writeSentenceStatistic() throws IOException {
            writeHeaderFor(KeyPrefix.SENTENCES);

            writeStatisticsFor(
                    KeyPrefix.SENTENCES,
                    new String[]{"count", "minimum", "maximum", "minimum_length", "maximum_length", "average"},
                    null
            );
        }

        private void writeWordStatistic() throws IOException {
            writeHeaderFor(KeyPrefix.WORDS);

            writeStatisticsFor(
                    KeyPrefix.WORDS,
                    new String[]{"count", "minimum", "maximum", "minimum_length", "maximum_length", "average"},
                    null
            );
        }

        private void writeMoneyStatistic() throws IOException {
            writeHeaderFor(KeyPrefix.MONEY);

            writeStatisticsFor(
                    KeyPrefix.MONEY,
                    new String[]{"count", "minimum", "maximum", "average"},
                    NumberFormat.getCurrencyInstance(inputLocale)
            );
        }

        private void writeNumberStatistic() throws IOException {
            writeHeaderFor(KeyPrefix.NUMBERS);

            writeStatisticsFor(
                    KeyPrefix.NUMBERS,
                    new String[]{"count", "minimum", "maximum", "average"},
                    NumberFormat.getNumberInstance(outputLocale)
            );
        }

        private void writeDateStatistic() throws IOException {
            writeHeaderFor(KeyPrefix.DATES);

            writeStatisticsFor(
                    KeyPrefix.DATES,
                    new String[]{"count", "minimum", "maximum", "average"},
                    DateFormat.getDateInstance(DateFormat.FULL, outputLocale)
            );
        }

        private void writeStatisticsFor(final KeyPrefix prefix,
                                        final String[] actions,
                                        final Format formatter
        ) throws IOException {
            for (final String action : actions) {
                writer.write("\t%s: %s.".formatted(
                                bundle.getString(getKey(prefix, action)),
                                processAction(prefix, action, formatter)
                        )
                );
                writer.newLine();
            }
        }

        private String processAction(final KeyPrefix prefix, final String action, final Format formatter) {
            final StatisticsType<?> stats = statistics.get(prefix.getKeyPrefix());

            switch (action) {
                case "count":
                    return "%d (%d %s)".formatted(
                            stats.size(),
                            stats.uniqueSize(),
                            bundle.getString(getKey(KeyPrefix.COMMON, "different"))
                    );
                case "minimum": {
                    Object result = stats.min();

                    if (result == null) {
                        return bundle.getString(getKey(KeyPrefix.COMMON, "empty"));
                    }

                    return formatter == null ? ("\"" + result + "\"") :
                            formatter.format(result);
                } case "maximum": {
                    Object result = stats.max();

                    if (result == null) {
                        return bundle.getString(getKey(KeyPrefix.COMMON, "empty"));
                    }

                    return formatter == null ? ("\"" + result + "\"") :
                            formatter.format(result);
                } case "minimum_length": {
                    Object result = stats.min_by_length();

                    if (result == null) {
                        return bundle.getString(getKey(KeyPrefix.COMMON, "empty"));
                    }

                    return "%d (\"%s\")".formatted(
                            result.toString().length(),
                            result
                    );
                } case "maximum_length": {
                    Object result = stats.max_by_length();

                    if (result == null) {
                        return bundle.getString(getKey(KeyPrefix.COMMON, "empty"));
                    }

                    return "%d (\"%s\")".formatted(
                            result.toString().length(),
                            result
                    );
                } case "average": {
                    Double result = stats.average();

                    if (result.equals(-1.0)) {
                        return bundle.getString(getKey(KeyPrefix.COMMON, "empty"));
                    }

                    return formatter == null ? result.toString() : formatter.format(result);
                } default:
                    throw new IllegalArgumentException("No such action: " + action);
            }
        }

        private void writeHeaderFor(final KeyPrefix key) throws IOException {
            writer.write(bundle.getString("%s.%s".formatted(
                    key.getKeyPrefix(),
                    "header")
            ));
            writer.newLine();
        }

        private static String getKey(final KeyPrefix prefix, final String postfix) {
            return "%s.%s".formatted(
                    prefix.getKeyPrefix(),
                    postfix
            );
        }
    }
}
