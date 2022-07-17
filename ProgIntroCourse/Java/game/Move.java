package game;

public class Move {
    private final int row;
    private final int col;
    private final Cell value;
    private final GameSettings s;

    public Move(int row, int col, Cell value, GameSettings s) {
        this.row = row;
        this.col = col;
        this.value = value;
        this.s = s;
    }

    public boolean isValid(Position position) {
        return 0 <= row && row < s.getM()
                && 0 <= col && col < s.getN()
                && position.getCell(row, col) == Cell.E;
    }

    public int getRow() {
        return row;
    }

    public int getCol() {
        return col;
    }

    public Cell getValue() {
        return value;
    }

    @Override
    public String toString() {
        return String.format("Move(%s, %d, %d)", value, row + 1, col + 1);
    }
}
