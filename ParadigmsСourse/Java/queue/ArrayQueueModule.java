package queue;


import java.util.Objects;

public class ArrayQueueModule {
    // immutable(n): for i = 0..n - 1: data'[(tale' + i) % data.length] = data[(tale + i) % data.length]

    private static Object[] data = new Object[2];
    private static int head = -1;
    private static int tail = -1;
    private static int size;

    // Pred: element != null
    // Post: (tale' = tale) && (head' = (tale + size) % data.length) && (size' = size + 1) &&
    //       (data[(tale + size) % data.length] = element) && immutable(size)
    public static void enqueue(final Object element) {
//        Objects.requireNonNull(element);
        assert element != null;
        if (tail == -1) {
            tail = 0;
        }
        checkCapacity(size + 1);
        head = (head + 1) % data.length;
        data[head] = element;
        size++;
    }

    // Pred: size >= 0
    // Post: immutable(size) && (size < data.length) && (head >= tale) &&
    //       (size' = size) && (head - tale + 1 == size)
    private static void checkCapacity(int size) {
        if (data.length < size) {
            Object[] temp = new Object[size * 2];
            for (int i = 0; i < ArrayQueueModule.size; i++) {
                temp[i] = data[(tail + i) % data.length];
            }
            data = temp;
            tail = 0;
            head = ArrayQueueModule.size - 1;
        }
    }

    // Pred: size >= 0
    // Post: immutable(size) && (size < data.length) && (head >= tale) &&
    //       (size' = size) && (head - tale + 1 == size)
    private static void checkDecrease() {
        if (size <= data.length / 4) {
            Object[] temp = new Object[data.length / 2];
            for (int i = 0; i < size; i++) {
                temp[i] = data[(tail + i) % data.length];
            }
            tail = 0;
            head = size - 1;
            data = temp;
        }
    }

    // Pred: size >= 1
    // Post: (for i = 0..size' - 1: data'[(tale' + i) % data.length] = data[(tale + i + 1) % data.length] &&
    //       (size' = size - 1) && (tale' = (tale + 1) % data.length) && (head' = head) &&
    //       (for i = 0..size' - 1: data'[(tale' + i) % data.length] = data[(tale + i + 1) % data.length])
    //       R = data[tale]
    public static Object dequeue() {
        assert size >= 1;
        size--;
        Object element = data[tail];
        data[tail] = null;
        if (isEmpty()) {
            tail = -1;
            head = -1;
        } else {
            tail = (tail + 1) % data.length;
            checkDecrease();
        }
        return element;
    }

    // Pred: (element != null)
    // Post: (tale' = (tale - 1) < 0 ? data.length - 1 : tale - 1) && (head' = head) &&
    //       (size' = size = 1) && (data'[tale'] = element) &&
    //       (for i = 0..size - 1: data'[(tale' + i + 1) % data.length] = data[(tale + i) % data.length])
    public static void push(Object element) {
        assert element != null;
        if (head == -1) {
            head = data.length - 1;
        }
        checkCapacity(size + 1);
        tail = (tail - 1) < 0 ? data.length - 1 : tail - 1;
        data[tail] = element;
        size++;
    }

    // Pred: size >= 1
    // Post: (for i = 0..size' - 1: data'[(tale' + i) % data.length] = data[(tale + i) % data.length] &&
    //       (size' = size - 1) && (tale' = tale) % data.length) && (head' = (tale + size' - 1) % data.length) &&
    //       (data'[tale] = null) && (R == data[head]);
    public static Object remove() {
        assert size >= 1;
        size--;
        Object element = data[head];
        data[head] = null;
        if (isEmpty()) {
            tail = -1;
            head = -1;
        } else {
            head = (tail + size - 1) % data.length;
            checkDecrease();
        }
        return element;
    }

    // Pred: (target != null)
    // Post: (R = min({i : deque[i] == target})) && immutable(size) && (head' = head) && (tale' = tale)
    //       where deque = [data[(tale + i) % data.length] for i = 0..size - 1]
    public static int indexOf(Object target) {
        return search(target, 0, 1);
    }

    // Pred: (target != null)
    // Post: (R = max({i : deque[i] == target})) && immutable(size) && (head' = head) && (tale' = tale) &&
    //       where deque = [data[(tale + i) % data.length] for i = 0..size - 1]
    public static int lastIndexOf(Object target) {
        return search(target, size - 1, -1);
    }

    // Pred: (target != null)
    // Post: immutable(size) && (head' = head) && (tale' = tale) &&
    //       (((step == -1) && (R = max({i : deque[i] == target}))) ||
    //       ((step == 1) && (R = min({i: deque[i] == target}))))
    //       where deque = [data[(tale + i) % data.length] for i = 0..size - 1]
    private static int search(Object target, int start, int step) {
        Object[] deque = toArray();
        int res = -1;
        int i = start;
        while (i >= 0 && i < size) {
            if (Objects.equals(target, deque[i])) {
                res = i;
                break;
            }
            i += step;
        }
        return res;
    }

    // Pred: target != null;
    // Post: (R = |{data[i] : data[i] == target}|) &&
    //       immutable(size) && (head' = head) && (tale' = tale)
    public static int count(Object target) {
//        Objects.requireNonNull(element);
        assert target != null;
        int c = 0;
        Object[] deque = toArray();
        for (int i = 0; i < size; i++) {
            if (Objects.equals(target, deque[i])) {
                c += 1;
            }
        }
        return c;
    }

    // Pred: size >= 0
    // Post: (R = [data[(tale + i) % data.length] for i = 0..size - 1]) && (size' = size) &&
    //       immutable(size) && (head' = head) && (tale' = tale)
    public static Object[] toArray() {
        if (size == 0) {
            return new Object[0];
        }
        Object[] res = new Object[size];
        for (int i = 0; i < size; i++) {
            res[i] = data[(tail + i) % data.length];
        }
        return res;
    }

    // Pred: true
    // Post: (size' = 0) && (tale' = -1) && (head' = -1)
    public static void clear() {
        data = new Object[2];
        size = 0;
        tail = -1;
        head = -1;
    }

    // Pred: true
    // Post: (R = data[tale]) && immutable(size) && (head' = head) && (tale' = tale) && (size' = size)
    public static Object element() {
        assert size >= 1;
        return data[tail];
    }

    // Pred: true
    // Post: R = data[head] && immutable(size) && (head' = head) && (tale' = tale) && (size' = size)
    public static Object peek() {
        assert size >= 1;
        return data[head];
    }

    // Pred: true
    // Post: (R = size) && immutable(size) && (head' = head) && (tale' = tale) && (size' = size)
    public static int size() {
        return size;
    }

    // Pred: true
    // Post: (R = (size == 0)) && immutable(size) && (head' = head) && (tale' = tale) && (size' = size)
    public static boolean isEmpty() {
        return size == 0;
    }

    // Pred: true
    // Post: (R = [data[i] for i = 0..data.length - 1]) && immutable(size) &&
    //       (head' = head) && (tale' = tale) && (size' = size)
    public static Object[] getArray() {
        return data;
    }
}