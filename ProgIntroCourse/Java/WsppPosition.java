import java.nio.charset.StandardCharsets;
import java.util.*;
import java.io.*;

public class WsppPosition {
	public static void main(String[] args) {
		MyScanner lineScanner;
		MyScanner wordScanner;
		BufferedWriter writer;
		Map<String, IntList> stat = new LinkedHashMap<>();
		int lineCounter = 0;
		int wordCounter;
		try {
			lineScanner = new MyScanner(new File(args[0]), "utf8");
			while (lineScanner.hasNextLine()) {
				lineCounter += 1;
				wordCounter = 0;
				String line = lineScanner.nextLine();
				wordScanner = new MyScanner(line);
				while (wordScanner.hasNextWord()) {
					wordCounter += 1;
					String word = wordScanner.nextWord().toLowerCase();
					if (stat.containsKey(word)) {
						addWord(stat.get(word), lineCounter, wordCounter);
					} else {
						stat.put(word, new IntList(new int[]{1, lineCounter, wordCounter}));
					}
				}
				wordScanner.close();
			}
			lineScanner.close();
		} catch (FileNotFoundException e) {
			System.err.println("File with this name was not found" + e.getMessage());
		} catch (NoSuchElementException e) {
			System.err.println("No such word no source " + e.getMessage());
		} catch (UnsupportedEncodingException e) {
			System.err.println("Wrong encoding" + e.getMessage());
		} catch (IOException e) {
			System.err.println("IOException " + e.getMessage());
		}
		try {
			writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), StandardCharsets.UTF_8));
			for (String word : stat.keySet()) {
				IntList temp = stat.get(word);
				StringBuilder sb = new StringBuilder(word + " " + temp.get(0) + " ");
				for (int j = 1; j < temp.size(); j += 2) {
					sb.append(temp.get(j) + ":" + temp.get(j + 1) + " ");
				}
				writer.write(sb.toString().strip() + System.lineSeparator());
			}
			writer.close();
		} catch (FileNotFoundException e) {
			System.err.println("File not found exception" + e.getMessage());
		} catch (IOException e) {
			System.err.println("Input Output Exception" + e.getMessage());
		}
	}

	public static void addWord(IntList list, int lineCounter, int wordCounter) {
		list.increment(0);
		list.add(lineCounter);
		list.add(wordCounter);
	}
}