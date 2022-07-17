package game;

import java.util.Arrays;
import java.util.Map;

public class TicTacToeBoard implements Board, Position {
    private static final Map<Cell, String> CELL_TO_STRING = Map.of(
            Cell.E, "·",
            Cell.X, "X",
            Cell.O, "0"
    );

    private final Cell[][] field;
    private final int m;
    private final int n;
    private final int k;
    private static Cell turn;

    public TicTacToeBoard(GameSettings s) {
        this.n = s.getN();
        this.m = s.getM();
        this.k = s.getK();
        this.field = new Cell[m][n];
        this.turn = Cell.X;
        for (Cell[] row : field) {
            Arrays.fill(row, Cell.E);
        }
    }

    @Override
    public Cell getTurn() {
        return turn;
    }

    @Override
    public Position getPosition() {
        return this;
    }

    @Override
    public GameResult makeMove(Move move) {
        if (!move.isValid(getPosition())) {
            System.out.println("Player made the wrong move");
            return GameResult.LOOSE;
        }

        field[move.getRow()][move.getCol()] = move.getValue();
        if (checkWin()) {
            return GameResult.WIN;
        }

        if (checkDraw()) {
            return GameResult.DRAW;
        }

        turn = turn == Cell.X ? Cell.O : Cell.X;
        return GameResult.UNKNOWN;
    }

    private boolean checkDraw() {
        int count = 0;
        for (int r = 0; r < m; r++) {
            for (int c = 0; c < n; c++) {
                if (field[r][c] == Cell.E) {
                    count++;
                }
            }
        }
        return count == 0;
    }

    private boolean checkWin() {
        int[][][] temp = new int[m][n][4];
        for (int i = 0; i < field.length; i++) {
            for (int j = 0; j < field[i].length; j++) {
                if (field[i][j] == turn) {
                    Arrays.fill(temp[i][j], 1);
                    if (j > 0) {
                        temp[i][j][0] += temp[i][j - 1][0];
                    }
                    if (i > 0) {
                        temp[i][j][1] += temp[i - 1][j][1];
                    }
                    if (i > 0 && j > 0) {
                        temp[i][j][2] += temp[i - 1][j - 1][2];
                    }
                    if (i > 0 && j < m - 1) {
                        temp[i][j][3] += temp[i - 1][j][3];
                    }
                    for (int c = 0; c < 4; c++) {
                        if (temp[i][j][c] == k) {
                            return true;
                        }
                    }
                } else {
                    Arrays.fill(temp[i][j], 0);
                }
            }
        }
        return false;
    }

    @Override
    public Cell getCell(int row, int column) {
        return field[row][column];
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder(" ".repeat(Integer.toString(m).length() - 1));
        for (int i = 1; i < n + 1; i++) {
            sb.append("  ").append(i);
        }
        sb.append(System.lineSeparator());
        for (int r = 0; r < m; r++) {
            sb.append(r + 1).append(" ".repeat(Integer.toString(m).length() - Integer.toString(r + 1).length() + 1));
            for (int c = 0; c < n; c ++) {
                int temp = Integer.toString(c + 1).length() - 1;
                int antemp = Integer.toString(r + 1).length() - 1;
                sb.append(CELL_TO_STRING.get(field[r][c])).append("  ").append(" ".repeat(temp));
            }
            sb.append(System.lineSeparator());
        }
        sb.setLength(sb.length() - System.lineSeparator().length());
        return sb.toString();
    }
}
