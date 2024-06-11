package utils;

import org.antlr.v4.runtime.*;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;

public class FileInputUtil {

    public static CharStream loadFileFromResources(String resourceName, Charset charset) throws IOException {
        try (InputStream inputStream = FileInputUtil.class
                .getClassLoader().getResourceAsStream(resourceName)) {
            if (inputStream == null) {
                throw new IOException("Resource not found: " + resourceName);
            }
            return CharStreams.fromStream(inputStream, charset);
        }
    }
}
