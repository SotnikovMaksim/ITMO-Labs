import java.util.*;
import java.io.*;

public class MyScanner {
	private int position = 0;
	private final Reader reader;
	private int readerResult = 0;
	private char[] buffer;
	private final int size;
	private boolean eof = false;
	private boolean readed = true;
	private String word = "";
	private boolean flagWord = false;


	public MyScanner(String source) throws IOException {
		this(source, source.length());
	}

	public MyScanner(String source, int size) throws IOException {
		this.reader = new StringReader(source);
		this.size = size;
		input();
	}

	public MyScanner(File source, String charset) throws IOException {
		this(source, 1024, charset);
	}

	public MyScanner(File source, int size, String charset) throws IOException {
		this.reader = new InputStreamReader(new FileInputStream(source), charset);
		this.size = size;
		input();
	}

	public MyScanner(InputStream source) throws IOException {
		this(source, 1024);
	}

	public MyScanner(InputStream source, int size) throws IOException {
		this.reader = new InputStreamReader(source);
		this.size = size;
		input();
	}

	public boolean hasNext() {
		return !eof;
	}

	public String next() throws NoSuchElementException, IOException {
		if (!eof) {
			StringBuilder sb = new StringBuilder();
			while (Character.isWhitespace(buffer[position]) && !eof) {
				move();
			}
			while (!(Character.isWhitespace(buffer[position]) || eof || buffer[position] == 0)) {
				sb.append(buffer[position]);
				move();
			}
			String nextItem = sb.toString();
			move();
			return nextItem;
		} else {
			throw new NoSuchElementException();
		}
	}

	public boolean hasNextLine() {
		return !eof;
	}
	
	public boolean hasNextWord() throws IOException {
		if (readed) {
			try {
				String word = nextWord();
				if (word.isEmpty()) {
					flagWord = false;
					readed = false;
					return false;
				}
				flagWord = true;
				readed = false;
				this.word = word;
				return true;
			} catch (NoSuchElementException e) {
				flagWord = false;
				readed = false;
				return false;
			}
		} else {
			return flagWord;
		}
	}

	public String nextWord() throws IOException {
		if (word.isEmpty()) {
			if (!eof) {
				StringBuilder sb = new StringBuilder();
				while (!(Character.isLetter(buffer[position]) ||  Character.DASH_PUNCTUATION == Character.getType(buffer[position]) || (buffer[position] == "'".charAt(0)) || ((buffer[position] == '-') && sb.length() != 0)) && !eof) {
		            move();
		        }
		        while (Character.isLetter(buffer[position]) ||  Character.DASH_PUNCTUATION == Character.getType(buffer[position]) || (buffer[position] == "'".charAt(0)) || ((buffer[position] == '-') && sb.length() != 0)) {
		        	sb.append(buffer[position]);
		        	move();
		        }
		        String nextItem = sb.toString();
		        move();
		        if (!nextItem.isEmpty()) {
		        	return nextItem;
		        } else {
		        	return "";
		        }
			} else {
		    	return "";
		    }
		} else {
			String temp = word;
			word = "";
			readed = true;
			return temp;
		}
	}

	public String nextLine() throws IOException {
        StringBuilder sb = new StringBuilder();
        while (!endln() && !eof) {
            sb.append(buffer[position]);
            move();
        }
        move();
        return sb.toString();
    }

    public boolean hasNextInt() {
    	return !eof;
    }

    public int nextInt() throws NoSuchElementException, NumberFormatException, IOException {
		try {
			String nextItem = next();
			int number = Integer.parseInt(nextItem);
			return number;
		} catch (NoSuchElementException e) {
			throw new NoSuchElementException();
		} catch (NumberFormatException e) {
			throw new NumberFormatException();
		}
    }

    private void input() throws IOException {
        if (!eof) {
        	if (size == 0) {
        		eof = true;
        		return;
        	}
        	try {
        		buffer = new char[size];
        		readerResult = reader.read(buffer, 0, size);
        	} catch (IOException e) {
        		throw new IOException();
        	}
        }
    }

	private boolean endln() throws IOException {
		if (position < size - 1) {
			if (buffer[position] == '\r' && buffer[position + 1] == '\n') {
				move();
				return true;
			}
			return buffer[position] == '\n';
		} else {
			if (buffer[position] == '\n') {
				return true;
			} else if (buffer[position] == '\r') {
				move();
				return buffer[position] == '\n';
			}
			return false;
		}
	}

    private void move() throws IOException {
    	if (!eof) {
    		position += 1;
    		if (position == readerResult) {
    			position = 0;
    			input();
    			if (readerResult == -1) {
    				eof = true;
    			}
    		}
    	}
    }

    public void close() throws IOException {
        try {
            reader.close();
        } catch (IOException e) {
            throw new NoSuchElementException();
        }
    }
}
