package queue;


public class ArrayQueueModuleTest {
    private static final String v = "value";

    public static void main(String[] args) {
        System.out.println("just filled queue: ");
        for (int i = 0; i < 10; i++) {
            ArrayQueueModule.enqueue(v + "_" + (i + 1));
        }
        dumpQueue();

        System.out.println("\nremove half from tale: ");
        for (int i = 0; i < 5; i++) {
            ArrayQueueModule.dequeue();
        }
        dumpQueue();

        refill();
        System.out.println("\nremove half from head: ");
        for (int i = 0; i < 5; i++) {
            ArrayQueueModule.remove();
        }
        dumpQueue();

        refill();
        System.out.println("\ncount of element before and after clear. Also check a size: ");
        System.out.println("Queue size: " + ArrayQueueModule.size());
        System.out.println("Count of 'value_6' in queue before clear: " + ArrayQueueModule.count("value_6"));
        ArrayQueueModule.clear();
        System.out.println("clear()");
        System.out.println("Queue size: " + ArrayQueueModule.size());
        System.out.println("Count of 'value_6' in queue after clear: " + ArrayQueueModule.count("value_6"));

        refill();
        System.out.println("\ncheckout indexOf() == lastIndexOf() for unique element in queue: ");
        int ind = ArrayQueueModule.indexOf("value_5");
        int lind = ArrayQueueModule.lastIndexOf("value_5");
        System.out.printf("indexOf() == lastIndexOf() - %s (%d %d)%n", ind == lind, ind, lind);
        dumpQueue();

        refill();
        ArrayQueueModule.enqueue("value_5");
        System.out.println("\ncheckout indexOf() != lastIndexOf() for non-unique element in queue: ");
        ind = ArrayQueueModule.indexOf("value_5");
        lind = ArrayQueueModule.lastIndexOf("value_5");
        System.out.printf("indexOf() != lastIndexOf() - %s (%d, %d)%n", ind != lind, ind, lind);
        dumpQueue();
    }

    public static void refill() {
        ArrayQueueModule.clear();
        for (int i = 0; i < 10; i++) {
            ArrayQueueModule.enqueue(v + "_" + (i + 1));
        }
    }

    private static void dumpQueue() {
        Object[] elements = ArrayQueueModule.toArray();
        System.out.print("Queue: ");
        for (int i = 0; i < ArrayQueueModule.size(); i++) {
            System.out.print(elements[i] + " ");
        }
        System.out.print(". Size: " + ArrayQueueModule.size() + "\n");
    }
}