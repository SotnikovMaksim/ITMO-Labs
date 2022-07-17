package queue;

import java.util.Objects;
import java.util.function.Predicate;

public abstract class AbstractQueue implements Queue {
    protected int size;

    public Object dequeue() {
        assert size >= 1;
        Object element = element();
        size--;
        dequeueImpl();
        return element;
    }

    public Object remove() {
        assert size >= 1;
        Object element = peek();
        size--;
        removeImpl();
        return element;
    }

    public void push(Object element) {
        assert element != null;
        pushImpl(element);
        size++;
    }

    public Object element() {
        assert size >= 1;
        return elementImpl();
    }

    public Object peek() {
        assert size >= 1;
        return peekImpl();
    }

    public void enqueue(Object element) {
        assert element != null;
        enqueueImpl(element);
        size++;
    }

    public int countIf(Predicate<Object> target) {
        int c = 0;
        Object[] data = toArray();
        for (int i = 0; i < size; i++) {
            if (target.test(data[i])) {
                c += 1;
            }
        }
        return c;
    }

    public int count(Object target) {
        int c = 0;
        Object[] data = toArray();
        for (int i = 0; i < size; i++) {
            if (Objects.equals(data[i], target)) {
                c += 1;
            }
        }
        return c;
    }

    public int indexOf(Object target) {
        return search(target, 0, 1);
    }

    public int lastIndexOf(Object target) {
        return search(target, size - 1, -1);
    }

    // Pred: (target != null)
    // Post: immutable(size) && (head' = head) && (tale' = tale) &&
    //       (((step == -1) && (R = max({i : deque[i] == target}))) ||
    //       ((step == 1) && (R = min({i: deque[i] == target}))))
    //       where deque = [data[(tale + i) % data.length] for i = 0..size - 1]
    protected int search(Object target, int start, int step) {
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

    public Object[] toArray() {
        if (size == 0) {
            return new Object[0];
        }
        Object[] res = new Object[size];
        toArrayImpl(res);
        return res;
    }

    public void clear() {
        size = 0;
        clearImpl();
    }

    public int size() {
        return size;
    }

    public boolean isEmpty() {
        return size == 0;
    }

    protected abstract void dequeueImpl();

    protected abstract void removeImpl();

    protected abstract Object elementImpl();

    protected abstract Object peekImpl();

    protected abstract void enqueueImpl(Object element);

    protected abstract void pushImpl(Object element);

    protected abstract void clearImpl();

    protected abstract void toArrayImpl(Object[] res);
}
