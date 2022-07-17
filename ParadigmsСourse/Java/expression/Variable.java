package expression;


public class Variable implements Common {
    public final String name;


    public Variable(String name) {
        this.name = name;
    }

    public int evaluate(int value) {
        return value;
    }

    public int evaluate(int xValue, int yValue, int zValue) {
        switch (name) {
            case "x":
                return xValue;
            case "y":
                return yValue;
            case "z":
                return zValue;
            default:
                return 0;
        }
    }

    public String toString() {
        return name;
    }

    @Override
    public int hashCode() {
        return Integer.hashCode(name.charAt(0));
    }

    public boolean equals(Object exp) {

        if (exp == null) {
            return false;
        }
        if (exp.getClass() != this.getClass()) {
            return false;
        }

        return this.hashCode() == exp.hashCode();
    }
}
