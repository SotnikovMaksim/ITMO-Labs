package info.kgeorgiy.ja.sotnikov.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ScalarIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.security.InvalidParameterException;
import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * A class that provides various parallel operations on lists using iterative parallelism.
 * The class is designed to process a list using the specified number of threads.
 * The provided operations include maximum, minimum, all, any, and count.
 *
 * @author Sotnikov Maksim (alone.sotnikov@mail.ru)
 */
public class IterativeParallelism implements ScalarIP {

    private final ParallelMapper parallelMapper;

    /**
     * Constructs a new IterativeParallelism instance with no ParallelMapper.
     */
    public IterativeParallelism() {
        this.parallelMapper = null;
    }

    /**
     * Constructs a new IterativeParallelism instance with a specified ParallelMapper.
     *
     * @param parallelMapper the ParallelMapper instance to be used for parallelism.
     */
    public IterativeParallelism(ParallelMapper parallelMapper) {
        this.parallelMapper = parallelMapper;
    }

    @Override
    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return parallelEvaluating(
                threads,
                values,
                x -> x.max(comparator).orElseThrow(),
                x -> x.max(comparator).orElseThrow()
        );
    }

    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return parallelEvaluating(
                threads,
                values,
                x -> x.min(comparator).orElseThrow(),
                x -> x.min(comparator).orElseThrow()
        );
    }

    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return Boolean.TRUE.equals(parallelEvaluating(
                threads,
                values,
                x -> x.allMatch(predicate),
                x -> x.allMatch(value -> value)
        ));
    }

    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return Boolean.TRUE.equals(parallelEvaluating(
                threads,
                values,
                x -> x.anyMatch(predicate),
                x -> x.anyMatch(value -> value)
        ));
    }

    @Override
    public <T> int count(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return Objects.requireNonNull(parallelEvaluating(
                threads,
                values,
                x -> x.filter(predicate).count(),
                x -> x.reduce(0L, Long::sum)
        )).intValue();
    }


    private <T, R> R parallelEvaluating(int threads, List<? extends T> values,
                                        Function<Stream<? extends T>, R> mapper,
                                        Function<Stream<R>, R> generalMapper) throws InterruptedException {
        if (values.isEmpty()) {
            throw new InvalidParameterException("Provided list doesn't have any elements");
        }

        if (threads == 0) {
            throw new InvalidParameterException("Provided count of accessible threads equals 0!");
        }

        int q = values.size() / threads;
        int r = values.size() % threads;
        int normalizedThreads = Math.min(values.size(), threads);

        List<List<? extends T>> subLists = new ArrayList<>(normalizedThreads);
        List<R> preAnswer;

        int globalLeft = 0;
        for (int i = 0; i < normalizedThreads; i++) {
            final int left = globalLeft;
            final int right = globalLeft + q + (r-- > 0 ? 1 : 0);

            subLists.add(values.subList(left, right));
            globalLeft = right;
        }

        if (Objects.isNull(parallelMapper)) {
            preAnswer = new ArrayList<>(Collections.nCopies(normalizedThreads, null));
            List<Thread> listOfThreads = new ArrayList<>(threads);

            for (int i = 0; i < normalizedThreads; i++) {
                final int index = i;

                listOfThreads.add(new Thread(() -> {
                    preAnswer.set(index, mapper.apply(subLists.get(index).stream()));
                }));

                listOfThreads.get(i).start();
            }

            for (Thread thread : listOfThreads) {
                thread.join();
            }
        } else {
            preAnswer = this.parallelMapper.map((List<? extends T> list) -> mapper.apply(list.stream()), subLists);
        }

        return generalMapper.apply(preAnswer.stream());

    }
}
