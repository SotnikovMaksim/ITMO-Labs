package queue;


public class LinkedQueue extends AbstractQueue {
    private Node head;
    private Node tail;

    private static class Node {
        private final Object element;
        private Node prev;
        private Node next;

        public Node(Object element, Node prev, Node next) {
            this.element = element;
            this.prev = prev;
            this.next = next;
        }
    }

    @Override
    protected void enqueueImpl(final Object element) {
        if (size == 0) {
            head = new Node(element, tail, null);
            tail = head;
        } else {
            head.next = new Node(element, head, null);
            head = head.next;
        }
    }

    @Override
    protected void dequeueImpl() {
        tail = tail.next;
        if (tail != null) {
            tail.prev = null;
        } else {
            head = null;
        }
    }

    @Override
    protected void pushImpl(Object element) {
        if (size == 0) {
            head = new Node(element, tail, null);
            tail = head;
        } else {
            tail.prev = new Node(element, null, tail);
            tail = tail.prev;
        }
    }

    @Override
    protected void removeImpl() {
        head = head.prev;
        if (head != null) {
            head.next = null;
        } else {
            tail = null;
        }
    }

    @Override
    protected void toArrayImpl(Object[] res) {
        Node iterator = tail;
        for (int i = 0; i < size; i++) {
            res[i] = iterator.element;
            iterator = iterator.next;
        }
    }

    @Override
    protected void clearImpl() {
        head = null;
        tail = null;
    }


    @Override
    protected Object elementImpl() {
        return tail.element;
    }


    @Override
    protected Object peekImpl() {
        return head.element;
    }
}
