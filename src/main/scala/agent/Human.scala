package agent

import game.Card
import scala.io.StdIn._
import game.Raise
import game.Move
import game.Fold
import game.Check
import game.Call
import agent.Agent
import game.Round

class Human(val name: String) extends Agent {
  private var moneyLeft = 10000
  var hand = null.asInstanceOf[List[Card]]
  
  def getMove(round: Round, minBet: Int, minIncrement: Int): Move = {
    println()
    println("----------------")
    println(s"$name What will you do?")
    println(s"Hand: ${hand.mkString(", ")}")
    println(s"River: ${round.getRiver().mkString(", ")}")
    println("What's your move?")
    if (minBet != 0)
      println(s"Minimum bet is $minBet")

    var move = null.asInstanceOf[Move]
    while (move == null) {
      val input = readLine().toLowerCase
      input.split(" ").head match {
        case "raise" =>
          try {
            val amt = input.split(" ")(1).toInt
            if (amt > moneyLeft) {
              println("cannot bet that much")
            } else if (amt < minIncrement) {
              println("too small of a raise")
            } else {
              moneyLeft -= amt
              move = Raise(amt)
            }
          } catch {
            case e: Exception => println("betting require a number")
          }

        case "call" =>
          if (minBet > moneyLeft) {
            moneyLeft = 0
            move = Call(moneyLeft)
          } else {
            moneyLeft -= minBet
            move =  Call(minBet)
          }

        case "fold" =>
          move = Fold()

        case "check" =>
          if (minBet > 0) {
            println("cannot check")
          } else {
            move = Check()
          }
      }
    }
    return move
  }

  override def allIn(): Boolean = moneyLeft == 0

  override def pay(amt: Int): Unit = moneyLeft += amt

  override def dealHand(cards: List[Card]): Unit = {
    hand = cards
  }
}