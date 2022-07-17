import java.util.*;
import java.io.*;


public class ReverseAbc2 {
	public static void main(String[] args) throws IOException {
		List<String[]> lines = new ArrayList<>();
		MyScanner line = new MyScanner(System.in);
		String[] letters = {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"};
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
			String[] numbers = new String[counter];
			for (int i = 0; i < counter; i++) {
				StringBuilder number = new StringBuilder();
				String[] letter_number = words[i].split("");
				for (int j = 0; j < letter_number.length; j++) {
					if (letter_number[j].equals("-")) {
						number.append("-");
					} else {
						number.append(findIndex(letters, letter_number[j]));
					}
				}
				numbers[i] = number.toString();
			}
			lines.add(numbers);
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

	private static int findIndex(String[] elements, String target) {
		for (int i = 0; i < elements.length; i++) {
			if (elements[i].equals(target)) {
				return i;
			}
		}
		return -1;
	}
}