package game;

public interface Position {
    Cell getTurn();

    Cell getCell(int row, int column);
}
