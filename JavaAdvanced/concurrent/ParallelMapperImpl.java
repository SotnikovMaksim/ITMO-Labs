package info.kgeorgiy.ja.sotnikov.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;

/**
 * This class provides a parallel implementation of the ParallelMapper interface.
 * It uses multiple threads to concurrently apply a given function to a list of
 * input elements, producing a list of result elements.
 *
 * @author Sotnikov Maksim (alone.sotnikov@mail.ru)
 */
public class ParallelMapperImpl implements ParallelMapper {

    private final int threadsCount;
    private final List<Thread> threadsList;
    private final Queue<Runnable> queue;

    /**
     * Constructs a new ParallelMapperImpl with the specified number of threads.
     *
     * @param threads the number of threads to be used for parallelism.
     */
    public ParallelMapperImpl(final int threads) {
        this.threadsCount = threads;
        this.queue = new ArrayDeque<>();
        this.threadsList = new ArrayList<>(threads);

        initializeThreads();
    }

    /**
     * Initializes the worker threads that will be used to process tasks.
     */
    private void initializeThreads() {
        for (int i = 0; i < threadsCount; i++) {
            threadsList.add(new Thread(() -> {
                try {
                    while (!Thread.interrupted()) {
                        Runnable function;

                        synchronized (queue) {
                            while (queue.isEmpty()) {
                                queue.wait();
                            }

                            function = queue.poll();

                        }

                        function.run();
                    }
                } catch (InterruptedException ignored) {
                }
            }));

            threadsList.get(i).start();
        }
    }

    @Override
    public <T, R> List<R> map(final Function<? super T, ? extends R> f, final List<? extends T> args) throws InterruptedException {
        ResultViewer<T, R> viewer = new ResultViewer<>();

        return viewer.run(f, args);
    }

    @Override
    public void close() {
        boolean interrupted = false;

        for (final Thread thread : threadsList) {
            thread.interrupt();

            while (true) {
                try {
                    thread.join();
                    break;
                } catch (InterruptedException e) {
                    System.err.println("Main thread interrupted: " + e.getMessage());
                    interrupted = true;
                }
            }
        }

        if (interrupted) {
            Thread.currentThread().interrupt();
        }
    }

    /**
     * This class is responsible for managing the results of concurrent function applications
     * and synchronizing access to the shared result list.
     *
     * @param <T> the type of elements in the input list.
     * @param <R> the type of elements in the result list.
     */
    private class ResultViewer<T, R> {
        /**
         * Counter of tasks.
         */
        private Integer counter = 0;

        /**
         * Runs the specified function on the input list concurrently, using the provided queue
         * to manage tasks and synchronize access to the result list.
         *
         * @param f    the function to be applied to each element of the input list.
         * @param args the input list of elements to be processed.
         * @return a list of result elements obtained by applying the function to each input element.
         * @throws InterruptedException if any thread has been interrupted during execution.
         */
        private List<R> run(final Function<? super T, ? extends R> f, final List<? extends T> args) throws InterruptedException {
            List<R> result = new ArrayList<>(Collections.nCopies(args.size(), null));

            synchronized (queue) {

                counter = args.size();

                for (int i = 0; i < args.size(); i++) {
                    final int index = i;

                    queue.add(() -> {
                        try {
                            result.set(index, f.apply(args.get(index)));
                        } catch (RuntimeException ignored) {
                        }

                        synchronized (this) {
                            if (--counter == 0) {
                                this.notify();
                            }
                        }
                    });

                    queue.notify();
                }
            }

            synchronized (this) {
                while (counter > 0) {
                    wait();
                }
            }

            return result;
        }
    }
}

