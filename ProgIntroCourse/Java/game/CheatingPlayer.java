//package game;
//
//
//public class CheatingPlayer implements Player {
//    private final GameSettings s;
//
//    public CheatingPlayer(GameSettings s) {
//        this.s = s;
//    }
//
//    @Override
//    public Move makeMove(Position position) {
//        final TicTacToeBoard board = (TicTacToeBoard) position;
//        Move first = null;
//        for (int r = 0; r < s.getM(); r++) {
//            for (int c = 0; c < s.getN(); c++) {
//                final Move move = new Move(r, c, position.getTurn(), s);
//                if (move.isValid(position)) {
//                    if (first == null) {
//                        first = move;
//                    } else {
//                        board.makeMove(move);
//                    }
//                }
//            }
//        }
//        return first;
//    }
//}
