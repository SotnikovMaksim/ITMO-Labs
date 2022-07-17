import java.util.*;
import java.io.*;


public class Reverse {
    public static void main(String[] args) throws IOException {
        List<String[]> lines = new ArrayList<>();
        MyScanner line = new MyScanner(System.in);
        while (line.hasNextLine()) {
            String string = line.nextLine();
            MyScanner word = new MyScanner(string);
            String[] words = new String[10];
            int counter = 0;
            while (word.hasNext()) {
                if (counter == words.length - 1) {
                    words = Arrays.copyOf(words, words.length * 2);
                }
                String w = word.next();
                words[counter] = w;
                counter += 1;
            }
            word.close();
            lines.add(Arrays.copyOf(words, counter));
        }
        line.close();
        for (int i = lines.size() - 1; i >= 0; i--) {
            String[] l = lines.get(i);
            for (int j = l.length - 1; j >= 0; j--) {
                System.out.print(l[j] + " ");
            }
            System.out.println();
        }
    }
}