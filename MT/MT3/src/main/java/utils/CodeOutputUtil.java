package utils;

import java.util.List;

import static utils.SymbolsUtils.NEW_LINE;

public class CodeOutputUtil {

    private static final List<String> NECESSARY_LIBS = List.of(
    );

    public static String createJavaClassFromCode(String translatedCode, String className) {
        String classTemplate =
            String.join(NEW_LINE, NECESSARY_LIBS) + NEW_LINE +
            "public class %s {" + NEW_LINE +
            "%s" + NEW_LINE + // Translated code here
            "}" + NEW_LINE;

        return String.format(classTemplate, className, translatedCode);
    }
}
