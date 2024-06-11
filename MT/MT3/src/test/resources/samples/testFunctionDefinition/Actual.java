
public class Actual {
    public static void greeting(Object name) {
        System.out.println(("Hello, " + (String) name));
    }
        
    public static void main(String[] args) {
        greeting("World");
    }
}
