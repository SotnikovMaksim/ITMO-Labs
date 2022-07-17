package queue;

import static queue.ArrayQueueADT.*;


public class ArrayQueueADTTest {
    private static final String v = "value";

    public static void main(String[] args) {
        ArrayQueueADT q = create();

        System.out.println("just filled queue: ");
        for (int i = 0; i < 10; i++) {
            enqueue(q, v + "_" + (i + 1));
        }
        dumpQueue(q);

        System.out.println("\nremove half from tale: ");
        for (int i = 0; i < 5; i++) {
            dequeue(q);
        }
        dumpQueue(q);

        refill(q);
        System.out.println("\nremove half from head: ");
        for (int i = 0; i < 5; i++) {
            remove(q);
        }
        dumpQueue(q);

        refill(q);
        System.out.println("\ncount of element before and after clear. Also check a size: ");
        System.out.println("Queue size: " + size(q));
        System.out.println("Count of 'value_6' in queue before clear: " + count(q, "value_6"));
        clear(q);
        System.out.println("clear()");
        System.out.println("Queue size: " + size(q));
        System.out.println("Count of 'value_6' in queue after clear: " + count(q, "value_6"));

        refill(q);
        System.out.println("\ncheckout indexOf() == lastIndexOf() for unique element in queue: ");
        int ind = indexOf(q, "value_5");
        int lind = lastIndexOf(q, "value_5");
        System.out.printf("indexOf() == lastIndexOf() - %s (%d %d)%n", ind == lind, ind, lind);
        dumpQueue(q);

        refill(q);
        enqueue(q, "value_5");
        System.out.println("\ncheckout indexOf() != lastIndexOf() for non-unique element in queue: ");
        ind = indexOf(q, "value_5");
        lind = lastIndexOf(q, "value_5");
        System.out.printf("indexOf() != lastIndexOf() - %s (%d, %d)%n", ind != lind, ind, lind);
        dumpQueue(q);
    }

    public static void refill(ArrayQueueADT q) {
        clear(q);
        for (int i = 0; i < 10; i++) {
            enqueue(q, v + "_" + (i + 1));
        }
    }

    private static void dumpQueue(ArrayQueueADT q) {
        Object[] elements = toArray(q);
        System.out.print("Queue: ");
        for (int i = 0; i < size(q); i++) {
            System.out.print(elements[i] + " ");
        }
        System.out.print(". Size: " + size(q) + "\n");
    }
}