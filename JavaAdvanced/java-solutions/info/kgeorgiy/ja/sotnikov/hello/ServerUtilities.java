package info.kgeorgiy.ja.sotnikov.hello;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

public class ServerUtilities {

    public static void closePool(ExecutorService pool) {
        boolean interrupted = false;
        pool.shutdown();

        while (true) {
            try {
                if (pool.awaitTermination(60, TimeUnit.SECONDS)) {
                    break;
                }
            } catch (InterruptedException ignored) {
                pool.shutdownNow();
                interrupted = true;
            }
        }

        if (interrupted) {
            Thread.currentThread().interrupt();
        }
    }
}
