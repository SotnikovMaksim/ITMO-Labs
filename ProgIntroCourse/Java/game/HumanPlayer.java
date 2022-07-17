package game;

import java.util.Scanner;

public class HumanPlayer implements Player {
    private final GameSettings s;
    private Scanner in;

    public HumanPlayer(Scanner in, GameSettings s) {
        this.in = in;
        this.s = s;
    }

    @Override
    public Move makeMove(Position position) {
        try {
            System.out.println("Enter your move for " + position.getTurn() + " in format 'row/column'");
            int row = in.nextInt() - 1, col = in.nextInt() - 1;
            Move move = new Move(row, col, position.getTurn(), s);
            if (!move.isValid(position)) {
                System.out.println("You entered wrong values. Please, try again: ");
                in = new Scanner(System.in);
                return makeMove(position);
            }
            return move;
        } catch (Exception e) {
            in.nextLine();
            return makeMove(position);
        }
    }
}
