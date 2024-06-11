package info.kgeorgiy.ja.sotnikov.i18n;

import java.text.Collator;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.function.ToIntFunction;

public class StringStatistics<E> implements StatisticsType<E> {

    private final Collator lexicographicalComparator;
    private final ToIntFunction<E> toIntFunction;
    private final Comparator<E> lengthComparator;
    private final List<E> data;

    public StringStatistics(List<E> data,
                            Collator lexicographicalComparator,
                            ToIntFunction<E> toIntFunction
    ) {
        this.lexicographicalComparator = lexicographicalComparator;
        this.lengthComparator = Comparator.comparingInt(toIntFunction);
        this.toIntFunction = toIntFunction;
        this.data = data;
    }

    @Override
    public int size() {
        return data.size();
    }

    @Override
    public int uniqueSize() {
        return new HashSet<>(data).size();
    }

    @Override
    public E min() {
        return data.stream().min(lexicographicalComparator).orElse(null);
    }

    @Override
    public E max() {
        return data.stream().max(lexicographicalComparator).orElse(null);
    }

    @Override
    public E min_by_length() {
        return data.stream().min(lengthComparator).orElse(null);
    }

    @Override
    public E max_by_length() {
        return data.stream().max(lengthComparator).orElse(null);
    }

    @Override
    public Double average() {
        return data.stream().mapToDouble(toIntFunction::applyAsInt).average().orElse(-1);
    }
}
