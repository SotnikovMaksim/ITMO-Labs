package info.kgeorgiy.ja.sotnikov.i18n;

import java.util.Comparator;
import java.util.HashSet;
import java.util.List;

public class NumericStatistics<E extends Number> implements StatisticsType<Number> {

    private final Comparator<E> comparator;
    private final List<E> data;

    public NumericStatistics(List<E> data, Comparator<E> comparator
    ) {
        this.comparator = comparator;
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
        return data.stream().min(comparator).orElse(null);
    }

    @Override
    public E max() {
        return data.stream().max(comparator).orElse(null);
    }

    @Override
    public E min_by_length() {
        throw new UnsupportedOperationException("This operation is not supported");
    }

    @Override
    public E max_by_length() {
        throw new UnsupportedOperationException("This operation is not supported");
    }

    @Override
    public Double average() {
        return data.stream().map(Number::doubleValue).mapToDouble(s -> s).average().orElse(-1);
    }
}
