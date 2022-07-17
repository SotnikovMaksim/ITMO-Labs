package game;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        final GameSettings settings = new GameSettings(new Scanner(System.in));
        Player player1 = new RandomPlayer(settings);
        Player player2 = new HumanPlayer(new Scanner(System.in), settings);
        int result;
        int played = 0;
        int matches = settings.getCountOfMatches();
        int[] score = new int[]{0, 0};


        while (played < matches) {
            System.out.println(played + 1 + " round");
            result = new TwoPlayerGame(
                    new TicTacToeBoard(settings),
                    player1,
                    player2
            ).play(true);

            switch (result) {
                case 1:
                    System.out.println("First player won in " + (played + 1) + " round");
                    score[0] += 1;
                    break;
                case 2:
                    System.out.println("Second player won in " + (played + 1) + " round");
                    score[1] += 1;
                    break;
                case 0:
                    System.out.println("Draw");
                    break;
                default:
                    System.out.println("Next turn");
            }
            player1 = (player1 instanceof RandomPlayer) ? (new HumanPlayer(new Scanner(System.in), settings)) : (new RandomPlayer(settings));
            player2 = (player1 instanceof HumanPlayer) ? (new RandomPlayer(settings)) : (new HumanPlayer(new Scanner(System.in), settings));
            played += 1;
            score = new int[]{score[1], score[0]};
        }
        score = matches % 2 == 1 ? new int[]{score[1], score[0]} : score;
        if (score[0] > score[1]) {
            System.out.println("Player won");
        } else if (score[0] < score[1]) {
            System.out.println("User won");
        } else {
            System.out.println("Draw. Nobody won");
        }
    }
}
