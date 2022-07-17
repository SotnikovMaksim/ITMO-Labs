package queue;


public class ArrayQueue extends AbstractQueue {
    // immutable(n): for i = 0..n - 1: data'[(tale' + i) % data.length] = data[(tale + i) % data.length]

    private Object[] data = new Object[2];
    private int head = -1;
    private int tail = -1;

    @Override
    protected void enqueueImpl(final Object element) {
        if (tail == -1) {
            tail = 0;
        }
        checkCapacity(size + 1);
        head = (head + 1) % data.length;
        data[head] = element;
    }

    // Pred: size >= 0
    // Post: immutable(size) && (size < data.length) && (head >= tale) &&
    //       (size' = size) && (head - tale + 1 == size)
    private void checkCapacity(int size) {
        if (data.length < size) {
            Object[] temp = new Object[size * 2];
            for (int i = 0; i < this.size; i++) {
                temp[i] = data[(tail + i) % data.length];
            }
            data = temp;
            tail = 0;
            head = this.size - 1;
        }
    }

    // Pred: size >= 0
    // Post: immutable(size) && (size < data.length) && (head >= tale) &&
    //       (size' = size) && (head - tale + 1 == size)
    private void checkDecrease() {
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

    @Override
    protected void dequeueImpl() {
        data[tail] = null;
        if (isEmpty()) {
            tail = -1;
            head = -1;
        } else {
            tail = (tail + 1) % data.length;
            checkDecrease();
        }
    }

    @Override
    protected void pushImpl(Object element) {
        if (head == -1) {
            head = data.length - 1;
        }
        checkCapacity(size + 1);
        tail = (tail - 1) < 0 ? data.length - 1 : tail - 1;
        data[tail] = element;
    }

    @Override
    protected void removeImpl() {
        data[head] = null;
        if (isEmpty()) {
            tail = -1;
            head = -1;
        } else {
            head = (tail + size - 1) % data.length;
            checkDecrease();
        }
    }

    @Override
    protected void toArrayImpl(Object[] res) {
        for (int i = 0; i < size; i++) {
            res[i] = data[(tail + i) % data.length];
        }
    }

    @Override
    protected void clearImpl() {
        data = new Object[2];
        head = -1;
        tail = -1;
    }

    @Override
    protected Object elementImpl() {
        return data[tail];
    }

    @Override
    protected Object peekImpl() {
        return data[head];
    }
}
