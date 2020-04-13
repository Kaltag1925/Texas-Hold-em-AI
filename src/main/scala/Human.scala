import agent.Agent
import scala.io.StdIn._

class Human extends Agent {
  private var moneyLeft
  val hand = null;
  
  def getMove(game: Game): (Move.Value, Int) = {
    System.out.println("What's your move?")

    var m = ""

    while (m != "bet" && m != "fold" && m != "check") {
      if (m == check && game.getMinimumRaise != 0) {
        System.out.println("Cannot check")
        m = ""
      }
      m = readLine().toLowerCase()
    }

    if (m == "bet") {
      System.out.println("How much?")
      var bet = readInt()
      while (bet < game.getMinimumRaise) {
        System.out.println("Raise must be at least " + game.getMinimumRaise + " and no more than " + moneyLeft)
        bet = readInt();
      }
      (Move.Bet, bet)
    } else if (m == "fold") {
      Move.Fold
    } else {
      Move.Check
    }
  }

}