package queue;


import java.util.Objects;

public class ArrayQueueADT {
    // immutable(n): for i = 0..n - 1: data'[(tale' + i) % data.length] = data[(tale + i) % data.length]

    private Object[] data = new Object[2];
    private int head = -1;
    private int tail = -1;
    private int size;

    public static ArrayQueueADT create() {
        final ArrayQueueADT q = new ArrayQueueADT();
        q.data = new Object[2];
        q.head = -1;
        q.tail = -1;
        return q;
    }

    // Pred: (element != null) && (queue != null)
    // Post: (tale' = tale) && (head' = (tale + size) % data.length) && (size' = size + 1) &&
    //       (data[(tale + size) % data.length] = element) && immutable(size)
    public static void enqueue(final ArrayQueueADT queue, final Object element) {
//        Objects.requireNonNull(element);
        assert element != null;
        if (queue.tail == -1) {
            queue.tail = 0;
        }
        checkCapacity(queue, queue.size + 1);
        queue.head = (queue.head + 1) % queue.data.length;
        queue.data[queue.head] = element;
        queue.size++;
    }

    // Pred: size >= 0
    // Post: immutable(size) && (size < data.length) && (head >= tale) &&
    //       (size' = size) && (head - tale + 1 == size)
    private static void checkCapacity(ArrayQueueADT queue, int size) {
        if (queue.data.length < size) {
            Object[] temp = new Object[size * 2];
            for (int i = 0; i < queue.size; i++) {
                temp[i] = queue.data[(queue.tail + i) % queue.data.length];
            }
            queue.data = temp;
            queue.tail = 0;
            queue.head = queue.size - 1;
        }
    }

    // Pred: size >= 0
    // Post: immutable(size) && (size < data.length) && (head >= tale) &&
    //       (size' = size) && (head - tale + 1 == size)
    private static void checkDecrease(ArrayQueueADT queue) {
        if (queue.size <= queue.data.length / 4) {
            Object[] temp = new Object[queue.data.length / 2];
            for (int i = 0; i < queue.size; i++) {
                temp[i] = queue.data[(queue.tail + i) % queue.data.length];
            }
            queue.tail = 0;
            queue.head = queue.size - 1;
            queue.data = temp;
        }
    }

    // Pred: (size >= 1) && (queue != null)
    // Post: (for i = 0..size' - 1: data'[(tale' + i) % data.length] = data[(tale + i + 1) % data.length] &&
    //       (size' = size - 1) && (tale' = (tale + 1) % data.length) && (head' = head) &&
    //       (for i = 0..size' - 1: data'[(tale' + i) % data.length] = data[(tale + i + 1) % data.length])
    //       R = data[tale]
    public static Object dequeue(final ArrayQueueADT queue) {
        assert queue.size >= 1;
        queue.size--;
        Object element = queue.data[queue.tail];
        queue.data[queue.tail] = null;
        if (isEmpty(queue)) {
            queue.tail = -1;
            queue.head = -1;
        } else {
            queue.tail = (queue.tail + 1) % queue.data.length;
            checkDecrease(queue);
        }
        return element;
    }

    // Pred: (element != null) && (queue != null)
    // Post: (tale' = (tale - 1) < 0 ? data.length - 1 : tale - 1) && (head' = head) &&
    //       (size' = size = 1) && (data'[tale'] = element) &&
    //       (for i = 0..size - 1: data'[(tale' + i + 1) % data.length] = data[(tale + i) % data.length])
    public static void push(final ArrayQueueADT queue, Object element) {
        assert element != null;
        if (queue.head == -1) {
            queue.head = queue.data.length - 1;
        }
        checkCapacity(queue, queue.size + 1);
        queue.tail = (queue.tail - 1) < 0 ? queue.data.length - 1 : queue.tail - 1;
        queue.data[queue.tail] = element;
        queue.size++;
    }

    // Pred: (size >= 1) && (queue != null)
    // Post: (for i = 0..size' - 1: data'[(tale' + i) % data.length] = data[(tale + i) % data.length] &&
    //       (size' = size - 1) && (tale' = tale) % data.length) && (head' = (tale + size' - 1) % data.length) &&
    //       (data'[tale] = null) && (R == data[head]);
    public static Object remove(final ArrayQueueADT queue) {
        assert queue.size >= 1;
        queue.size--;
        Object element = queue.data[queue.head];
        queue.data[queue.head] = null;
        if (isEmpty(queue)) {
            queue.tail = -1;
            queue.head = -1;
        } else {
            queue.head = (queue.tail + queue.size - 1) % queue.data.length;
            checkDecrease(queue);
        }
        return element;
    }

    // Pred: (target != null) && (queue != null)
    // Post: (R = min({i : deque[i] == target})) && immutable(size) && (head' = head) && (tale' = tale)
    //       where deque = [data[(tale + i) % data.length] for i = 0..size - 1]
    public static int indexOf(final ArrayQueueADT queue, Object target) {
        return search(queue, target, 0, 1);
    }

    // Pred: (target != null) && (queue != null)
    // Post: (R = max({i : deque[i] == target})) && immutable(size) && (head' = head) && (tale' = tale) &&
    //       where deque = [data[(tale + i) % data.length] for i = 0..size - 1]
    public static int lastIndexOf(final ArrayQueueADT queue, Object target) {
        return search(queue, target, queue.size - 1, -1);
    }

    // Pred: (target != null) && (queue != null)
    // Post: immutable(size) && (head' = head) && (tale' = tale) &&
    //       (((step == -1) && (R = max({i : deque[i] == target}))) ||
    //       ((step == 1) && (R = min({i: deque[i] == target}))))
    //       where deque = [data[(tale + i) % data.length] for i = 0..size - 1]
    private static int search(final ArrayQueueADT queue, Object target, int start, int step) {
        Object[] deque = toArray(queue);
        int res = -1;
        int i = start;
        while (i >= 0 && i < queue.size) {
            if (Objects.equals(target, deque[i])) {
                res = i;
                break;
            }
            i += step;
        }
        return res;
    }

    // Pred: (target != null) && (queue != null)
    // Post: (R = |{data[i] : data[i] == target}|) &&
    //       immutable(size) && (head' = head) && (tale' = tale)
    public static int count(final ArrayQueueADT queue, Object target) {
        int c = 0;
        Object[] deque = toArray(queue);
        for (int i = 0; i < queue.size; i++) {
            if (Objects.equals(target, deque[i])) {
                c += 1;
            }
        }
        return c;
    }

    // Pred: (size >= 0) && (queue != null)
    // Post: (R = [data[(tale + i) % data.length] for i = 0..size - 1]) && (size' = size) &&
    //       immutable(size) && (head' = head) && (tale' = tale)
    public static Object[] toArray(final ArrayQueueADT queue) {
        if (queue.size == 0) {
            return new Object[0];
        }
        Object[] res = new Object[queue.size];
        for (int i = 0; i < queue.size; i++) {
            res[i] = queue.data[(queue.tail + i) % queue.data.length];
        }
        return res;
    }

    // Pred: (queue != null)
    // Post: (size' = 0) && (tale' = -1) && (head' = -1)
    public static void clear(final ArrayQueueADT queue) {
        queue.data = new Object[2];
        queue.size = 0;
        queue.tail = -1;
        queue.head = -1;
    }

    // Pred: (queue != null)
    // Post: (R = data[tale]) && immutable(size) && (head' = head) && (tale' = tale) && (size' = size)
    public static Object element(final ArrayQueueADT queue) {
        assert queue.size >= 1;
        return queue.data[queue.tail];
    }

    // Pred: (queue != null)
    // Post: R = data[head] && immutable(size) && (head' = head) && (tale' = tale) && (size' = size)
    public static Object peek(final ArrayQueueADT queue) {
        assert queue.size >= 1;
        return queue.data[queue.head];
    }

    // Pred: (queue != null)
    // Post: (R = size) && immutable(size) && (head' = head) && (tale' = tale) && (size' = size)
    public static int size(final ArrayQueueADT queue) {
        return queue.size;
    }

    // Pred: (queue != null)
    // Post: (R = (size == 0)) && immutable(size) && (head' = head) && (tale' = tale) && (size' = size)
    public static boolean isEmpty(final ArrayQueueADT queue) {
        return queue.size == 0;
    }
}
