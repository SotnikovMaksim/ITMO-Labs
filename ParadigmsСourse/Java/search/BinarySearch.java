package search;

public class BinarySearch {

    // Pred: (args - array of Strings) && (args.length > 0) && (args[i] contains Integer)
    //        && ((args[i] >= args[j] | i > j > 0))
    // Post: R = min {i | a[i] <= args[0]} - the minimum index of the element which less or equal than args[0]
    public static void main(String[] args) {
        // args.length > 0
        int x = Integer.parseInt(args[0]);
        int[] a = new int[args.length - 1];

        // (a.length = args.length - 1) && (x = Integer.parseInt(args[0]))
        // (a.length = args.length - 1)

        for (int i = 1; i < args.length; i++) {
            // (1 <= i && i < args.length) && (a.length = args.length - 1)
            a[i - 1] = Integer.parseInt(args[i]);
        }

        // (a.length = args.length - 1) && (x = Integer.parseInt(args[0])) &&
        // && (a = Integer.parseInt(args[i]), i = 1 ... args.length - 1) &&
        // && (a[i] >= a[j] | i > j)

//        int R = iteratedBinarySearch(x, a);
        int R = recursiveBinarySearch(x, a, -1, a.length);

        // (a.length = args.length - 1) && (x = Integer.parseInt(args[0])) &&
        // && (a = Integer.parseInt(args[i]), i = 1 ... args.length - 1)
        System.out.println(R);
    }

    // Pred: (a[i] >= a[j] | i > j)
    // Post: (R = right') && (right' = min {i | a[i] <= args[0]} - the minimum index of element of
    //       array 'a', that is less or equal than x)
    public static int iteratedBinarySearch(int x, int[] a) {
        int left = -1;
        int right = a.length;
        int middle;

        // (left = -1) && (right >= 0)
        // left < right
        while (right - left > 1) {
            // (right - left > 1) && (left < right)
            middle = (left + right) / 2;
            // middle <= (right - 2 + right) / 2
            // middle <= right - 1
            if (a[middle] > x) {
                // (right - left > 1) && (left < right) && (a[middle] > x)
                left = middle;
            } else {
                // (right - left > 1) && (left < right) && (a[middle] <= x)
                right = middle;
            }
            // (left = middle) || right = middle)
            // left < right
        }
        // (left < right) && (right - left <= 1)
        // (left = right - 1) -> the indexes have converged
        return right;
    }

    // Pred: (a[i] >= a[j] | i > j)
    // Post: (R = right') && (right' = min {i | a[i] <= args[0]} - the minimum index of element of array 'a', that is less or equal than x)
    public static int recursiveBinarySearch(int x, int[] a, int left, int right) {
        // left < right
        if (right - left == 1) {
            // (left == right - 1) && (left < right)
            return right;
        }
        // (right - left != 1) && (left < right)
        // right - 1 > left
        int middle = (left + right) / 2;
        // middle <= (right - 2 + right) / 2
        // middle <= right - 1
        if (a[middle] > x) {
            left = middle;
            // left <= right - 1
            // left < right
            return recursiveBinarySearch(x, a, left, right);
        } else {
            right = middle;
            // (right -= 1) && (right - 1 > left)
            // left < right
            return recursiveBinarySearch(x, a, left, right);
        }
    }
}
