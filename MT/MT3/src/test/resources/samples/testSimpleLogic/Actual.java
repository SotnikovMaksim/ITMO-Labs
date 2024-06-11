
public class Actual {
    public static void isEven(Object num) {
        if ((((Integer) num % 2) == 0)) {
            System.out.println("Even");
        } else {
            System.out.println("Odd");
        }
        ;
    }
    
        
    public static void main(String[] args) {
        isEven(4);
        isEven(1);
    }
}
