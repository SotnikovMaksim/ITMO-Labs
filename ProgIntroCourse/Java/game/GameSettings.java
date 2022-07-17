package game;

import java.util.Scanner;

public class GameSettings {
    private final int countOfMatches = 1;
    private Scanner in;
    private int n;
    private int m;
    private int k;

    public GameSettings(Scanner in) {
        this.in = in;
        input();
    }

    private void input() {
        try {
            System.out.println("Enter board size and condition for victory in format <m n k>: ");
            this.m = in.nextInt();
            this.n = in.nextInt();
            this.k = in.nextInt();
            if (!isValid(m, n, k)) {
                System.out.println("You entered inappropriate values. Try again:");
                in.nextLine();
                input();
            }
        } catch (Exception e) {
            System.out.println("You entered wrong values");
            in.nextLine();
            input();
        }
    }

    private boolean isValid(int m, int n, int k) {
        return m > 0 && n > 0 && k > 0;
    }

    public int getN() {
        return n;
    }

    public int getM() {
        return m;
    }

    public int getK() {
        return k;
    }

    public int getCountOfMatches() {
        return countOfMatches;
    }
}
