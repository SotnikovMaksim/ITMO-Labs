package info.kgeorgiy.ja.sotnikov.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Objects;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.stream.Collectors;
import java.util.zip.ZipException;

/**
 * Class for implementing interfaces.
 *
 * @author Sotnikov Maksim (alone.sotnikov@mail.ru)
 */
public class Implementor implements JarImpler {

    /**
     * Constant {@link String} representing return value of {@link System#lineSeparator()} method.
     */
    private static final String NEW_LINE = System.lineSeparator();

    /**
     * Constant {@link String} representing Tab Key.
     */
    private static final String FIRST_LEVEL = "\t";

    /**
     * Constant {@link String} representing Tab Key. Just doubled {@link #FIRST_LEVEL}.
     */
    private static final String SECOND_LEVEL = "\t".repeat(2);

    /**
     * Constant {@link String} representing Java file extension.
     */
    private static final String JAVA = ".java";

    /**
     * Constant {@link String} representing Java class file extension.
     */
    private static final String CLASS = ".class";

    /**
     * {@link Path} to a file in which to generate the implementation of interface.
     * Created in {@link #implement(Class, Path)} and already has {@code .java} extension.
     */

    private String implementationPath;

    /**
     * {@link String} representing name of file in which to generate the implementation of interface.
     */
    private String implementationName;

    /**
     * The main method of the Implementor program.
     * This method takes two command-line arguments: the name of an interface to implement
     * and a path to a directory in which to generate the implementation file.
     *
     * @param args the command-line arguments passed to the program.
     *             This should be an array with one string: the name of the interface to implement.
     * @throws ImplerException if the provided arguments are invalid or if there is an error during implementation.
     *                         This exception is thrown if:
     *                         - args is null or has length != 1
     *                         - args[0] is null
     *                         - the interface with the provided name does not exist
     *                         - the path provided is invalid
     */
    public static void main(String[] args) throws ImplerException {
        if (args == null || args.length < 1 || anyNull(args)) {
            System.err.println("Invalid arguments!"
                    + System.lineSeparator()
                    + Arrays.toString(args));
            return;
        } else if (args[0].equals("jar") && args.length != 3) {
            System.err.println("Invalid arguments for jar implementation!"
                    + System.lineSeparator()
                    + Arrays.toString(args));
            return;
        } else if (args.length != 1) {
            System.err.println("Invalid arguments for class implementation!"
                    + System.lineSeparator()
                    + Arrays.toString(args));
            return;
        }

        try {
            Implementor implementor = new Implementor();

            if (args[0].equals("-jar")) {
                implementor.implementJar(Class.forName(args[1]), Path.of(args[2]));
            } else {
                implementor.implement(Class.forName(args[0]), Path.of(args[0]));
            }
        } catch (ClassNotFoundException e) {
            throw new ImplerException("Interface with provided name does not exist: " + e);
        } catch (InvalidPathException e) {
            throw new ImplerException("Invalid path of provided interface: " + e);
        }
    }

    /**
     * Utility method for checking is there any element is {@code null} in provided array.
     *
     * @param array Array we are looking for null in.
     * @return Returns true if any element in array gives true on {@link Objects#isNull}, false otherwise.
     */
    private static boolean anyNull(String[] array) {
        for (final String s : array) {
            if (Objects.isNull(s)) {
                return true;
            }
        }

        return false;
    }

    @Override
    public void implement(Class<?> token, Path root) throws ImplerException {
        if (!token.isInterface()) {
            throw new ImplerException("Provided token is not interface: " + token);
        }

        if (Modifier.isPrivate(token.getModifiers())) {
            throw new ImplerException("Provided token is private interface: " + token);
        }

        Path directoryPath;

        try {
            directoryPath = root.resolve(token.getPackageName().replace('.', File.separatorChar));
        } catch (InvalidPathException e) {
            throw new ImplerException("Invalid path of subdirectories!");
        }

        try {
            Files.createDirectories(directoryPath);
        } catch (FileAlreadyExistsException e) {
            System.err.println("Subdirectories already exists.");
        } catch (IOException e) {
            throw new ImplerException("Failed to create subdirectories: " + directoryPath, e);
        }

        implementationName = token.getSimpleName() + "Impl";
        implementationPath = directoryPath + File.separator + implementationName;

        try (BufferedWriter writer = Files.newBufferedWriter(Path.of(implementationPath + JAVA), StandardCharsets.UTF_8)) {
            try {
                ClassImplementer implementer = new ClassImplementer(token, writer);
                implementer.implementClass();
            } catch (IOException e) {
                throw new ImplerException("Error occurred due generating class", e);
            }
        } catch (InvalidPathException e) {
            throw new ImplerException("Invalid path to file for implementation!", e);
        } catch (IOException e) {
            throw new ImplerException("Failed to create output file", e);
        }
    }

    @Override
    public void implementJar(Class<?> token, Path root) throws ImplerException {
        implement(token, root.getParent());

        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        final String classpath;

        if (Objects.isNull(compiler)) {
            throw new ImplerException("Could not find java compiler, include tools.jar to classpath");
        }

        try {
            classpath = Path.of(
                            token
                                    .getProtectionDomain()
                                    .getCodeSource()
                                    .getLocation()
                                    .toURI())
                    .toString();
        } catch (URISyntaxException e) {
            throw new ImplerException("Invalid root path: " + root);
        }

        final String[] args = new String[]{
                implementationPath + JAVA,
                "-cp",
                classpath,
                "-encoding",
                "utf8"
        };

        final int exitCode = compiler.run(null, null, null, args);

        if (exitCode != 0) {
            throw new ImplerException("Failed to compile Interface implementation! Exit code: " + exitCode); // Strange moment..
        }

        try (JarOutputStream writer = new JarOutputStream(Files.newOutputStream(root))) {
            writer.putNextEntry(new JarEntry(token.getPackageName().replace(".", "/") + "/" + implementationName + ".class"));
            Files.copy(Path.of(implementationPath + CLASS), writer);
//            writer.closeEntry();
        } catch (ZipException e) {
            throw new ImplerException("Failed to create zip entry during generating jar!!", e);
        } catch (IOException e) {
            throw new ImplerException("Something went wrong during generating jar file!", e);
        }
    }

    /**
     * A utility class used for implementing an interface by writing its implementation to a BufferedWriter.
     * This class is used by the Implementor class.
     *
     * @author Sotnikov Maksim (alone.sotnikov@mail.ru)
     */
    private class ClassImplementer {

        /**
         * The {@link BufferedWriter} to which the implementation will be written.
         */
        private final BufferedWriter writer;

        /**
         * The {@link Class} object representing the interface to be implemented.
         */
        final Class<?> token;

        /**
         * Constructs a new ClassImplementer object with the given Class object and {@link #writer}.
         *
         * @param token  The Class object representing the interface to be implemented.
         * @param writer The {@link BufferedWriter} to which the implementation will be written.
         * @throws IOException If there is an I/O error while using the BufferedWriter.
         */
        private ClassImplementer(Class<?> token, BufferedWriter writer) throws IOException {
            this.token = token;
            this.writer = writer;
        }

        /**
         * Implements the interface specified by the Class object and writes the implementation to the {@link #writer}.
         * This method writes the class header, followed by the implementation of each method in the interface.
         *
         * @throws IOException If there is an I/O error while writing to the {@link #writer}.
         */
        private void implementClass() throws IOException {
            writeHeader();

            for (Method m : token.getMethods()) {
                implementMethod(m);
            }

            writer.write("}");
            writer.close();
        }

        /**
         * Writes the implementation of the specified {@link Method} to the {@link #writer}.
         * This method writes the method header, and it's body. If provided method is static,
         * function do nothing.
         *
         * @param m The {@link Method} object representing the method to be implemented.
         * @throws IOException If there is an I/O error while writing to the {@link #writer}.
         */
        void implementMethod(Method m) throws IOException {
            if (Modifier.isStatic(m.getModifiers())) {
                return;
            }

            writer.write("@Override" + NEW_LINE);
            writer.write(FIRST_LEVEL + "public " + m.getReturnType().getCanonicalName() + " " + m.getName());

            String args = createArgsLine(m);

            writer.write(args + "{" + NEW_LINE);

            if (!m.getReturnType().equals(Void.TYPE)) {
                writer.write(SECOND_LEVEL + "return " + getDefault(m) + ";" + NEW_LINE);
            }

            writer.write(FIRST_LEVEL + "}" + NEW_LINE.repeat(2));
        }

        /**
         * Writes the class header to the {@link #writer}.
         * This method writes the package statement, followed by the class header.
         *
         * @throws IOException If there is an I/O error while writing to the {@link #writer}.
         */
        void writeHeader() throws IOException {
            String packageName = token.getPackageName();

            if (!packageName.equals("")) {
                writer.write("package " + packageName + ";" + NEW_LINE.repeat(2));
            }

            writer.write("class " + implementationName +
                    " implements " + token.getCanonicalName() + " {" + NEW_LINE.repeat(2)
            );
        }

        /**
         * Creates a String representing the comma-separated argument list for the specified {@link Method}.
         *
         * @param m The {@link Method} object representing the method for which to create the argument list.
         * @return A {@link String} representing the argument list for the specified Method.
         */
        String createArgsLine(Method m) {
            return Arrays.stream(m.getParameters())
                    .map(t -> t.getType().getCanonicalName() + " " + t.getName())
                    .collect(Collectors.joining(", ", "(", ")"));
        }

        /**
         * Returns the default value for the return type of the given method.
         *
         * @param m the {@link Method} object representing the method
         * @return the default value for the return type
         */
        String getDefault(Method m) {
            Class<?> returnType = m.getReturnType();

            if (returnType == void.class) {
                return "";
            } else if (returnType == boolean.class) {
                return "false";
            } else if (returnType.isPrimitive()) {
                return "0";
            } else {
                return "null";
            }
        }
    }
}
