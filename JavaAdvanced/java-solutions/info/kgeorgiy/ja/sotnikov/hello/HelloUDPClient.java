package info.kgeorgiy.ja.sotnikov.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketException;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Phaser;

public class HelloUDPClient implements HelloClient {

    private ExecutorService requestsPool;

    public static void main(String[] args) {
        if (
                Objects.isNull(args) ||
                        args.length != 5 ||
                        Arrays.stream(args).anyMatch(Objects::isNull)
        ) {
            throw new IllegalArgumentException("Invalid arguments!");
        }

        try {
            new HelloUDPClient().run(
                    args[0], Integer.parseInt(args[1]),
                    args[2], Integer.parseInt(args[3]),
                    Integer.parseInt(args[4])
            );
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid integer value in arguments!");
        }
    }

    @Override
    public void run(final String host, final int port, final String prefix, final int threads, final int requests) {
        requestsPool = Executors.newFixedThreadPool(threads);

        InetSocketAddress address;
        Phaser phaser = new Phaser(threads + 1);

        address = new InetSocketAddress(host, port);

        for (int i = 1; i <= threads; i++) {
            final int threadNumber = i;

            requestsPool.submit(() -> {
                final DatagramSocket client;
                int clientSocketBufferSize;

                try {
                    client = new DatagramSocket();
                    clientSocketBufferSize = client.getReceiveBufferSize();
                    client.setSoTimeout(200);
                } catch (SocketException e) {
                    System.err.println("Failed to create socket!");
                    phaser.arrive();
                    return;
                }

                for (int j = 1; j <= requests; j++) {
                    String serverResponseString = "";
                    String requestString = String.format("%s%d_%d", prefix, threadNumber, j);
                    String expected = "Hello, " + requestString;
                    byte[] requestBytes = requestString.getBytes();

                    DatagramPacket packet = new DatagramPacket(requestBytes, requestBytes.length, address);

                    do {
                        try {
                            client.send(packet);

                            DatagramPacket serverResponse = new DatagramPacket(new byte[clientSocketBufferSize], clientSocketBufferSize);
                            client.receive(serverResponse);
                            serverResponseString = new String(serverResponse.getData(), serverResponse.getOffset(), serverResponse.getLength());
                        } catch (IOException ignored) {
                            System.err.println("Error occurred while waiting for a response from the server");
                        }
                    } while (!serverResponseString.equals(expected));
                }

                client.close();
                phaser.arrive();
            });
        }

        phaser.arriveAndAwaitAdvance();
        ServerUtilities.closePool(requestsPool);
    }
}
