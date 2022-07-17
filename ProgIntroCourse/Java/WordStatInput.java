import java.util.*;
import java.io.*;

public class WordStatInput {
    public static void main(String[] args) {
        MyScanner sc;
        Writer writer;
        Map<String, Integer> stat = new LinkedHashMap<>();
        try {
            sc = new MyScanner(new File(args[0]), "utf8");
            while (sc.hasNextWord()) {
                String word = sc.nextWord().toLowerCase();
                if (stat.containsKey(word)) {
                    stat.put(word, stat.get(word) + 1);
                } else {
                    stat.put(word, 1);
                }
            }
            sc.close();
        } catch (FileNotFoundException e) {
            System.err.println("File not found exception" + e.getMessage());
        } catch (UnsupportedEncodingException e) {
            System.err.println("Unsupported encoding" + e.getMessage());
        } catch (NoSuchElementException e) {
            System.err.println("No such word no source " + e.getMessage());
        } catch (IOException e) {
            System.err.println("IOException " + e.getMessage());
        }
        try {
            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), "utf8"));
            for (String word : stat.keySet()) {
                writer.write(word + " " + stat.get(word) + System.lineSeparator());
            }
            writer.close();
        } catch (FileNotFoundException e) {
            System.err.println("File not found exception" + e.getMessage());
        } catch (IOException e) {
            System.err.println("Input Output Exception" + e.getMessage());
        }
    }
}
