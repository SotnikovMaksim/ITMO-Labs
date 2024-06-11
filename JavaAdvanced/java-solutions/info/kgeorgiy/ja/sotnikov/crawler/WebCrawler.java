package info.kgeorgiy.ja.sotnikov.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Parallel site crawler. Load links recursively.
 *
 * @author Sotnikov Maksim (alone.sotnikov@mail.ru)
 */
public class WebCrawler implements Crawler {

    private static final Integer DEFAULT_DEPTH = 2;
    private static final Integer DEFAULT_DOWNLOADS = 10;
    private static final Integer DEFAULT_EXTRACTORS = 10;
    private static final Integer DEFAULT_HOST_COUNT = 10;

    private final ExecutorService downloaderPool;
    private final ExecutorService extractorsPool;
    private final Downloader downloader;

    /**
     * Constructor to create a new WebCrawler object.
     *
     * @param downloader  the downloader responsible for downloading web pages
     * @param downloaders the number of downloaders in the downloader thread pool
     * @param extractors  the number of extractors in the extractor thread pool
     * @param perHost     the maximum number of downloads allowed per host
     */
    public WebCrawler(Downloader downloader, int downloaders, int extractors, int perHost) {
        this.downloaderPool = Executors.newFixedThreadPool(downloaders);
        this.extractorsPool = Executors.newFixedThreadPool(extractors);
        this.downloader = downloader;
    }

    public static void main(String[] args) {
        if (args == null || args.length == 0 || args.length > 4 || args[0] == null) {
            throw new IllegalArgumentException("Invalid arguments!");
        }

        int[] arguments = getArguments(args);

        try {
            Downloader downloader = new CachingDownloader(1, Path.of("sites"));

            try (Crawler crawler = new WebCrawler(downloader, arguments[1], arguments[2], arguments[3])) {
                Result result = crawler.download(args[0], arguments[0]);

                System.out.println("Downloaded links: " + result.getDownloaded());
                System.out.println("Errors caught during downloads: " + result.getErrors());
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static int[] getArguments(String[] args) {
        int[] arguments = new int[]{
                DEFAULT_DEPTH,
                DEFAULT_DOWNLOADS,
                DEFAULT_EXTRACTORS,
                DEFAULT_HOST_COUNT
        };

        for (int index = 1; index < args.length; index++) {
            if (Objects.nonNull(args[index])) {
                arguments[index - 1] = Integer.parseInt(args[index]);
            }
        }

        return arguments;
    }

    @Override
    public Result download(String url, int depth) {
        final Set<String> downloaded = ConcurrentHashMap.newKeySet();
        final Map<String, IOException> errors = new ConcurrentHashMap<>();

        new BFSCrawler(downloaded, errors, depth).run(url);

        return new Result(new ArrayList<>(downloaded), errors);
    }

    private class BFSCrawler {
        private final Map<String, IOException> errors;
        private final Set<String> downloaded;
        private final BlockingQueue<String> queue;
        private final int depth;
        private Phaser phaser;
        private AtomicInteger registered;

        BFSCrawler(final Set<String> downloaded, final Map<String, IOException> errors, int depth) {
            this.downloaded = downloaded;
            this.errors = errors;
            this.depth = depth;
            this.queue = new LinkedBlockingQueue<>();
        }

        private void run(String url) {
            phaser = new Phaser();
            registered = new AtomicInteger();

            phaser.register();
            registered.incrementAndGet();
            queue.add(url);
            downloaded.add(url);

            bfs();

            downloaded.removeAll(errors.keySet());
        }

        private void bfs() {
            int currentDepth = 1;

            while (currentDepth != depth + 1) {
                int currentRegistered = registered.get();
                registered.set(0);

                for (int i = 0; i < currentRegistered; i++) {
                    final String url = queue.poll();

                    phaser.register();
                    downloaderPool.submit(
                            createDownloader(url, currentDepth)
                    );
                }

                phaser.arriveAndAwaitAdvance();
                currentDepth++;
            }
        }

        private Runnable createDownloader(final String url, final int currentDepth) {
            return () -> {
                try {
                    Document document = downloader.download(url);

                    if (currentDepth + 1 <= depth) {
                        extractorsPool.submit(
                                createExecutor(document)
                        );
                    } else {
                        phaser.arriveAndDeregister();
                    }
                } catch (IOException e) {
                    phaser.arriveAndDeregister();
                    errors.put(url, e);
                }
            };
        }

        private Runnable createExecutor(Document document) {
            return () -> {
                try {
                    List<String> links = document.extractLinks();

                    for (final String link : links) {
                        if (downloaded.add(link)) {
                            registered.incrementAndGet();
                            queue.add(link);
                        }
                    }

                } catch (IOException ignored) {
                } finally {
                    phaser.arriveAndDeregister();
                }
            };
        }
    }

    @Override
    public void close() {
        closeFor(downloaderPool);
        closeFor(extractorsPool);
    }

    private static void closeFor(ExecutorService pool) {
        pool.shutdown();
        try {
            if (!pool.awaitTermination(60, TimeUnit.SECONDS)) {
                pool.shutdownNow();
                if (!pool.awaitTermination(60, TimeUnit.SECONDS)) {
                    System.err.println("Pool did not terminate");
                }
            }
        } catch (InterruptedException ex) {
            pool.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }
}
