package info.kgeorgiy.ja.sotnikov.walk;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;


public class Walk {
    private static final String ERROR_OCCURRED = "0".repeat(64);
    private static final int BUFFER_SIZE = 1 << 16;

    public static void main(String[] args) {
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.err.println("Invalid arguments!");
            return;
        }

        try {
            Path inputFile = Paths.get(args[0]);

            if (Files.notExists(inputFile) || !Files.isRegularFile(inputFile)) {
                System.err.println("Either input file is directory or it does not exist!");
                return;
            }

            try {
                Path outputFile = Paths.get(args[1]);
                Path parentPath = outputFile.getParent();

                if (parentPath != null && Files.notExists(outputFile.getParent())) {
                    System.err.println("Parent directory of output file path does not exist!");
                    Files.createDirectories(outputFile.getParent());
                }

                try (
                        BufferedReader fileNameReader = Files.newBufferedReader(inputFile, StandardCharsets.UTF_8);
                ) {
                    try (
                            BufferedWriter writer = Files.newBufferedWriter(outputFile, StandardCharsets.UTF_8);
                    ) {
                        String path;

                        while ((path = fileNameReader.readLine()) != null) {
                            hashFile(path, writer);
                        }
                    } catch (IOException e) {
                        System.err.println("Something went wrong due writing in output file!" + System.lineSeparator() + e.getMessage());
                    } catch (NoSuchAlgorithmException e) {
                        System.err.println("No such hash algorithm: " + e.getMessage());
                    }
                } catch (IOException e) {
                    System.err.println("Something went wrong due reading input file!" + System.lineSeparator() + e.getMessage());
                }
            } catch (InvalidPathException e) {
                System.err.println("Invalid output path: " + args[1]);
            } catch (IOException e) {
                System.err.println("Failed to create a path to output file's directory!" + System.lineSeparator() +
                        "Path: " + args[1]);
            }
        } catch (InvalidPathException e) {
            System.err.println("Invalid input path: " + args[0]);
        }
    }

    /**
     * @param path   Path to file you want to hash.
     * @param writer Stream for writing an answer
     * @throws IOException              Either file to hash does not exist or something went wrong due reading it
     * @throws NoSuchAlgorithmException Unsupported hash algorithm
     */
    private static void hashFile(String path, BufferedWriter writer) throws NoSuchAlgorithmException, IOException {
        try {
            Path pathToHash = Paths.get(path);
            byte[] buffer = new byte[BUFFER_SIZE];
            int c;

            if (!Files.isRegularFile(pathToHash)) {
                writer.write(ERROR_OCCURRED + " " + path);
                writer.newLine();
                System.err.println("Either path to hash is directory or it does not exist!");
                return;
            }

            try (
                    BufferedInputStream dataReader = new BufferedInputStream(Files.newInputStream(pathToHash))
            ) {
                MessageDigest digest = MessageDigest.getInstance("SHA-256");

                while ((c = dataReader.read(buffer, 0, BUFFER_SIZE)) > 0) {
                    digest.update(buffer, 0, c);
                }

                byte[] hash = digest.digest();
                writer.write(String.format("%0" + (hash.length << 1) + "x", new BigInteger(1, hash)) + " " + path);
                writer.newLine();
            } catch (IOException e) {
                writer.write(ERROR_OCCURRED + " " + path);
                writer.newLine();
            }
        } catch (InvalidPathException e) {
            writer.write(ERROR_OCCURRED + " " + path);
            writer.newLine();
            System.err.println("Invalid path to hashable file: " + path);
        }
    }
}