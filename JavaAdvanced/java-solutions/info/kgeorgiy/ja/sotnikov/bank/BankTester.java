package info.kgeorgiy.ja.sotnikov.bank;

import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;

public class BankTester {
    private final long start = System.currentTimeMillis();

    public static void main(String[] args) {
        new BankTester().run();
    }

    public void run() {
        test();

        System.out.println("=".repeat(25));
        final long time = System.currentTimeMillis() - start;
        System.out.printf("OK. Time: %d", time);
    }

    private void test() {
        System.err.println("Running...");

        final Result result = new JUnitCore().run(BankTests.class);
        if (result.wasSuccessful()) {
            return;
        }

        for (final Failure failure : result.getFailures()) {
            System.err.println("Test " + failure.getDescription().getMethodName() + " failed: " + failure.getMessage());
            if (failure.getException() != null) {
                failure.getException().printStackTrace();
            }
        }

        System.exit(1);
        throw new AssertionError("Exit");
    }
}

