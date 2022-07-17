package game;

import java.util.Random;

public class RandomPlayer implements Player {
    private final Random random = new Random();
    private final GameSettings s;

    public RandomPlayer(GameSettings s) {
        this.s = s;
    }

    @Override
    public Move makeMove(Position position) {
        final Move move = new Move(
                random.nextInt(s.getM()),
                random.nextInt(s.getN()),
                position.getTurn(), s
        );
        return move;
    }
}
