import java.util.*;

public class IntList{
	private int position;
	private int[] data;

	public IntList() {
		data = new int[10];
	}

	public IntList(int size) {
		data = new int[size];
	}

	public IntList(int[] source) {
		position = source.length - 1;
		data = Arrays.copyOf(source, position + 10);
	}

	public int size() {
		return position + 1;
	}

	public void changeSize(int size) {
		data = Arrays.copyOf(data, size);
		if (size < position) {
			position = size;
		}
	}

	public void clear() {
		data = new int[position];
		position = 0;
	}

	public Integer get(int index) {
		if (foolTest(index)) {
			return null;
		}
		return data[index];
	}

	public void remove(int index) {
		if (foolTest(index)) {
			return;
		}
		for (int i = index; i < position - 1; i++) {
			data[i] = data[i + 1];
		}
		position -= 1;
	}

	public void add(int value) {
		if (position == data.length - 1) {
			data = Arrays.copyOf(data, data.length + 10);
		}
		data[++position] = value;
	}

	public void add(int value, int index) {
		if (foolTest(index)) {
			return;
		}
		if (data.length == position + 2) {
			data = Arrays.copyOf(data, data.length + 10);
		}
		for (int i = data.length - 1; i > index - 1; i--) {
			data[i] = data[i - 1];
		}
		data[index] = value;
		position += 1;
	}

	public void add(int[] values) {
		int len = values.length;
		for (int i = 0; i < len; i++) {
			if (position == data.length - 1) {
				data = Arrays.copyOf(data, data.length + 10);
			}
			data[++position] = values[i];
		}
	}

	public void increment(int index) {
		if (foolTest(index)) {
			return;
		}
		data[index] = data[index] + 1;
	}

	public void decrement(int index) {
		if (foolTest(index)) {
			return;
		}
		data[index] = data[index + 1];
	}

	private boolean foolTest(int index) {
		return (index > position || index < 0);
	}
}