package info.kgeorgiy.ja.sotnikov.i18n;

public interface StatisticsType<E> {

    int size();

    int uniqueSize();

    E min();

    E max();

    E min_by_length();

    E max_by_length();

    Double average();
}
