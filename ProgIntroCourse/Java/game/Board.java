package game;

public interface Board {
    public Position getPosition();

    GameResult makeMove(Move move);
}
