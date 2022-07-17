package queue;


public class LinkedQueueTest {
    private static final String v = "value";

    public static void main(String[] args) {
        ArrayQueue q = new ArrayQueue() ;
        System.out.println("just filled queue: ");
        for (int i = 0; i < 10; i++) {
            q.enqueue(v + "_" + (i + 1));
        }
        dumpQueue(q);

        System.out.println("\nremove half from tale: ");
        for (int i = 0; i < 5; i++) {
            q.dequeue();
        }
        dumpQueue(q);

        refill(q);
        System.out.println("\nremove half from head: ");
        for (int i = 0; i < 5; i++) {
            q.remove();
        }
        dumpQueue(q);

        refill(q);
        System.out.println("\ncount of element before and after clear. Also check a size: ");
        System.out.println("Queue size: " + q.size());
        System.out.println("Count of 'value_6' in queue before clear: " + q.count("value_6"));
        q.clear();
        System.out.println("clear()");
        System.out.println("Queue size: " + q.size());
        System.out.println("Count of 'value_6' in queue after clear: " + q.count("value_6"));

        refill(q);
        System.out.println("\ncheckout indexOf() == lastIndexOf() for unique element in queue: ");
        int ind = q.indexOf("value_5");
        int lind = q.lastIndexOf("value_5");
        System.out.printf("indexOf() == lastIndexOf() - %s (%d %d)%n", ind == lind, ind, lind);
        dumpQueue(q);

        refill(q);
        q.enqueue("value_5");
        System.out.println("\ncheckout indexOf() != lastIndexOf() for non-unique element in queue: ");
        ind = q.indexOf("value_5");
        lind = q.lastIndexOf("value_5");
        System.out.printf("indexOf() != lastIndexOf() - %s (%d, %d)%n", ind != lind, ind, lind);
        dumpQueue(q);
    }

    public static void refill(ArrayQueue q) {
        q.clear();
        for (int i = 0; i < 10; i++) {
            q.enqueue(v + "_" + (i + 1));
        }
    }

    private static void dumpQueue(ArrayQueue q) {
        Object[] elements = q.toArray();
        System.out.print("Queue: ");
        for (int i = 0; i < q.size(); i++) {
            System.out.print(elements[i] + " ");
        }
        System.out.print(". Size: " + q.size() + "\n");
    }
}