package utils;

import java.nio.file.Files;
import java.nio.file.Path;;
import java.io.IOException;

public class FileOutputUtil {

    public static void writeToFile(String content, Path filePath) throws IOException {
        Files.write(filePath, content.getBytes());
    }
}
