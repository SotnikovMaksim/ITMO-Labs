import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class TranslatorTester {

    private final Path samplesPath = Path.of("samples");

    @AfterAll
    public static void cleanUp() {
        deleteClassFiles(new File(TesterUtils.RESOURCES_PATH));
    }

    @Test
    public void testVariableDefinition() throws IOException {
        String testName = "testVariableDefinition";
        testTemplate(testName);
    }

    @Test
    public void testFunctionDefinition() throws IOException {
        String testName = "testFunctionDefinition";
        testTemplate(testName);
    }

    @Test
    public void testSimpleLogic() throws IOException {
        String testName = "testSimpleLogic";
        testTemplate(testName);
    }

    private void testTemplate(String testName) throws IOException {
        Path sourcePath = samplesPath.resolve(testName).resolve("test.testlang");
        Path resultPath = samplesPath.resolve(testName).resolve("Actual.java");
        TesterUtils.translateFromSource(sourcePath, resultPath, "Actual");

        compareWithExpected(testName);
    }

    private void compareWithExpected(String testName) {
        compileTestDirectory(testName);

        try (URLClassLoader classLoader = createClassLoaderForTest(testName)) {
            Class<?> actualClass = Class.forName("Actual", true, classLoader);
            Class<?> expectedClass = Class.forName("Expected", true, classLoader);

            testDeclaredMethodNamesEqual(actualClass, expectedClass);
            testDeclaredMethodParameterTypesEqual(actualClass, expectedClass);
            testDeclaredMethodParameterNamesEqual(actualClass, expectedClass);
            testSameOutput(actualClass, expectedClass);

        } catch (ClassNotFoundException e) {
            throw new RuntimeException("Failed to found class");
        } catch (IOException e) {
            throw new RuntimeException("Failed to create class loader");
        }
    }

    private void testDeclaredMethodNamesEqual(Class<?> actualClass, Class<?> expectedClass) {
        Assertions.assertArrayEquals(
                Arrays.stream(actualClass.getDeclaredMethods())
                        .map(Method::getName)
                        .toArray(),
                Arrays.stream(expectedClass.getDeclaredMethods())
                        .map(Method::getName)
                        .toArray()
        );
    }

    private void testDeclaredMethodParameterTypesEqual(Class<?> actualClass, Class<?> expectedClass) {
        Assertions.assertArrayEquals(
                Arrays.stream(actualClass.getDeclaredMethods())
                        .map(Method::getParameterTypes)
                        .toArray(),
                Arrays.stream(expectedClass.getDeclaredMethods())
                        .map(Method::getParameterTypes)
                        .toArray()
        );
    }

    private void testDeclaredMethodParameterNamesEqual(Class<?> actualClass, Class<?> expectedClass) {
        Assertions.assertArrayEquals(
                Arrays.stream(actualClass.getDeclaredMethods())
                        .map(m -> Arrays.stream(m.getParameters())
                                .map(Parameter::getName)
                                .toArray())
                        .toArray(),
                Arrays.stream(expectedClass.getDeclaredMethods())
                        .map(m -> Arrays.stream(m.getParameters())
                                .map(Parameter::getName)
                                .toArray())
                        .toArray()
        );
    }

    private void testSameOutput(Class<?> actualClass, Class<?> expectedClass) {
        String expectedOutput = runMainAndGetOutput(expectedClass);
        String actualOutput = runMainAndGetOutput(actualClass);

        Assertions.assertEquals(actualOutput, expectedOutput);
    }

    private URLClassLoader createClassLoaderForTest(String testName) throws MalformedURLException {
        return URLClassLoader.newInstance(new URL[] { pathToTestDir(testName).toUri().toURL() } );
    }

    private void compileTestDirectory(String testName) {
        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();

        if (compiler == null) {
            throw new RuntimeException("Could not find java compiler, include tools.jar to classpath");
        }

        File directory = pathToTestDir(testName).toFile();
        File[] files = directory.listFiles((dir, name) -> name.endsWith(".java"));

        if (files != null) {
            List<String> compilerArgs = new ArrayList<>();
            compilerArgs.add("-encoding");
            compilerArgs.add("UTF-8");

            for (File file : files) {
                compilerArgs.add(file.getAbsolutePath());
            }

            int exitCode = compiler.run(null, null, null, compilerArgs.toArray(new String[0]));

            if (exitCode != 0) {
                throw new RuntimeException("Compilation error, exit code: " + exitCode);
            } else {
                System.out.println("Compilation successful.");
            }
        } else {
            throw new RuntimeException("No .java files found in the directory.");
        }
    }

    public static String runMainAndGetOutput(Class<?> clazz) {
        PrintStream originalOut = System.out;

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream newOut = new PrintStream(baos);

        System.setOut(newOut);

        try {
            Method main = clazz.getMethod("main", String[].class);

            String[] args = {};
            main.invoke(null, (Object) args);

            System.out.flush();
            System.setOut(originalOut);

            return baos.toString();
        } catch (NoSuchMethodException e) {
            System.setOut(originalOut);
            return "";
        } catch (Exception e) {
            System.setOut(originalOut);
            e.printStackTrace();
            return "";
        }
    }

    private Path pathToTestDir(String testName) {
        return Paths.get(TesterUtils.RESOURCES_PATH, "samples", testName);
    }

    public static void deleteClassFiles(File directory) {
        if (!directory.exists()) {
            System.out.println("Directory does not exist: " + directory.getPath());
            return;
        }

        File[] files = directory.listFiles();
        if (files != null) {
            for (File file : files) {
                if (file.isDirectory()) {
                    deleteClassFiles(file);
                } else if (file.getName().endsWith(".class")) {
                    boolean deleted = file.delete();
                    if (!deleted) {
                        System.out.println("Could not delete file: " + file.getPath());
                    }
                }
            }
        }
    }
}
