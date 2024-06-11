package generator;

import java.io.File;

public class Utility {

    public static void createDirectoryIfDoesntExist(String path) {
        if (!new File(path).exists()) {
            boolean success = new File(path).mkdir();

            if (!success) {
                throw new RuntimeException("Failed to create directory for generated code");
            }
        }
    }
}
