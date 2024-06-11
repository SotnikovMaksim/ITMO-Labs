package info.kgeorgiy.ja.sotnikov.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketAddress;
import java.net.SocketException;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.Scanner;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;


public class HelloUDPServer implements HelloServer {

    private static final byte[] QUEUE_OVERLOADED_MESSAGE_BYTES = ("503. Server is overloaded").getBytes();
    private static final int MAX_TASKS_IN_QUEUE = 1000;

    private ThreadPoolExecutor workersPool;
    private DatagramSocket server;

    private int serverSocketBufferSize;

    public static void main(String[] args) {
        if (
                Objects.isNull(args) ||
                        args.length != 2 ||
                        Objects.isNull(args[0]) ||
                        Objects.isNull(args[1])
        ) {
            throw new IllegalArgumentException("Invalid arguments!");
        }

        try (HelloServer server = new HelloUDPServer()) {
            System.out.println("Starting...");

            server.start(
                    Integer.parseInt(args[0]),
                    Integer.parseInt(args[1])
            );

            System.out.println("Server started successfully! (Enter \"close\" to close server)");
            closeWaiting(server);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid integer value in arguments!");
        }
    }

    private static void closeWaiting(HelloServer server) {
        Scanner sc = new Scanner(System.in);

        try {
            while (!sc.nextLine().equals("close")) {
                Thread.sleep(10);
            }
        } catch (InterruptedException e) {
            System.err.println("Thread was interrupted due server work");
        }

        server.close();
    }

    private boolean prepare(final int port, final int threads) {
        workersPool = (ThreadPoolExecutor) Executors.newFixedThreadPool(threads);

        try {
            server = new DatagramSocket(port);
            serverSocketBufferSize = server.getReceiveBufferSize();
        } catch (SocketException e) {
            return false;
        }

        return true;
    }

    @Override
    public void start(int port, int threads) {
        if (!prepare(port, threads)) {
            System.err.println("Failed to create socket!");
            return;
        }

        new Thread(() -> {
            while (!server.isClosed() && !workersPool.isShutdown()) {
                final DatagramPacket packet = new DatagramPacket(new byte[serverSocketBufferSize], serverSocketBufferSize);

                try {
                    server.receive(packet);
                } catch (IOException e) {
                    System.err.println(e.getMessage());
                    return;
                }

                if (workersPool.getQueue().size() > MAX_TASKS_IN_QUEUE) {
                    DatagramPacket errorPacket = new DatagramPacket(
                            QUEUE_OVERLOADED_MESSAGE_BYTES,
                            QUEUE_OVERLOADED_MESSAGE_BYTES.length,
                            packet.getSocketAddress()
                    );

                    try {
                        server.send(errorPacket);
                    } catch (IOException ignored) {
                        System.err.println("Failed to send error message");
                    }

                    continue;
                }

                workersPool.submit(() -> {
                    String request = new String(packet.getData(), packet.getOffset(), packet.getLength(), StandardCharsets.UTF_8);
                    SocketAddress clientSocket = packet.getSocketAddress();
                    byte[] response = ("Hello, " + request).getBytes();

                    DatagramPacket responsePacket = new DatagramPacket(response, response.length, clientSocket);

                    try {
                        server.send(responsePacket);
                    } catch (IOException ignored) {
                        System.err.println("Failed to send response");
                    }
                });
            }
        }).start();
    }

    @Override
    public void close() {
        server.close();
        ServerUtilities.closePool(workersPool);
    }
}
