package queue;

import java.util.function.Predicate;

public interface Queue {
    // immutable(n): for i = 0..n - 1: data'[(tale' + i) % data.length] == data[(tale + i) % data.length]

    // Pred: element != null
    // Post: (tale' == tale) && (head' == (tale + size) % data.length) && (size' == size + 1) &&
    //       (data[(tale + size) % data.length] == element) && immutable(size)
    void enqueue(Object element);

    // Pred: (element != null)
    // Post: (tale' == (tale - 1) < 0 ? data.length - 1 : tale - 1) && (head' == head) &&
    //       (size' == size == 1) && (data'[tale'] == element) &&
    //       (for i = 0..size - 1: data'[(tale' + i + 1) % data.length] == data[(tale + i) % data.length])
    void push(Object element);

    // Pred: size >= 1
    // Post: (for i = 0..size' - 1: data'[(tale' + i) % data.length] == data[(tale + i + 1) % data.length] &&
    //       (size' == size - 1) && (tale' == (tale + 1) % data.length) && (head' == head) &&
    //       (for i = 0..size' - 1: data'[(tale' + i) % data.length] == data[(tale + i + 1) % data.length])
    //       R == data[tale]
    Object dequeue();

    // Pred: size >= 1
    // Post: (for i = 0..size' - 1: data'[(tale' + i) % data.length] == data[(tale + i) % data.length] &&
    //       (size' == size - 1) && (tale' == tale) % data.length) && (head' == (tale + size' - 1) % data.length) &&
    //       (data'[tale] == null) && (R == data[head]);
    Object remove();

    // Pred: true
    // Post: (R == data[tale]) && immutable(size) && (head' == head) && (tale' == tale) && (size' == size)
    Object element();

    // Pred: true
    // Post: R == data[head] && immutable(size) && (head' == head) && (tale' == tale) && (size' == size)
    Object peek();

    // Pred: size >= 0
    // Post: (R == [data[(tale + i) % data.length] for i = 0..size - 1]) && (size' == size) &&
    //       immutable(size) && (head' == head) && (tale' == tale)
    Object[] toArray();

    // Pred: target != null;
    // Post: (R == |{data[i] : data[i] == target}|) &&
    //       immutable(size) && (head' == head) && (tale' == tale)
    int countIf(Predicate<Object> target);

    // Pred: target != null;
    // Post: (R == |{data[i] : data[i] == target}|) &&
    //       immutable(size) && (head' == head) && (tale' == tale)
    int count(Object target);

    // Pred: true
    // Post: (size' == 0) && (tale' == -1 || tale' == null) && (head' == -1 || head' == null)
    void clear();

    // Pred: true
    // Post: (R == size) && immutable(size) && (head' == head) && (tale' == tale) && (size' == size)
    int size();

    // Pred: true
    // Post: (R == (size == 0)) && immutable(size) && (head' == head) && (tale' == tale) && (size' == size)
    boolean isEmpty();

    // Pred: (target != null)
    // Post: (R == min({i : deque[i] == target})) && immutable(size) && (head' == head) && (tale' == tale)
    //       where deque == [data[(tale + i) % data.length] for i = 0..size - 1]
    int indexOf(Object target);

    // Pred: (target != null)
    // Post: (R == max({i : deque[i] == target})) && immutable(size) && (head' == head) && (tale' == tale) &&
    //       where deque == [data[(tale + i) % data.length] for i = 0..size - 1]
    int lastIndexOf(Object target);
}
