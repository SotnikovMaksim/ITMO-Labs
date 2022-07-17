package game;

public class SequentialPlayer implements Player {
    private final GameSettings s;

    public SequentialPlayer(GameSettings s) {
        this.s = s;
    }

    @Override
    public Move makeMove(Position position) {
        for (int r = 0; r < s.getN(); r++) {
            for (int c = 0; c < s.getM(); c++) {
                final Move move = new Move(r, c, position.getTurn(), s);
                if (move.isValid(position)) {
                    return move;
                }
            }
        }
        throw new AssertionError("No valid moves");
    }
}
