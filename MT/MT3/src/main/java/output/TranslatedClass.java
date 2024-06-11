
public class TranslatedClass {
    public static Object gcd(Object a, Object b) {
        if (((Integer) b == 0)) {
            return a;
        } else {
            return gcd(b, ((Integer) a % (Integer) b));
        }

    }
    public static Object mod(Object a, Object b) {
        return ((Integer) a % (Integer) b);
    }
    
        
    public static void main(String[] args) {
        gcd(1023, 858);
        System.out.println(mod(10, 5));
    }
}
