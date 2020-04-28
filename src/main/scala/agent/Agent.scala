package agent

import game.Card
import game.Move
import game.Round

trait Agent {
  def getMove(round: Round, minAmt: Int, minimumRaise: Int): Move
  def allIn(): Boolean
  def hand: List[Card]
  def pay(amt: Int): Unit
  def dealHand(cards: List[Card]): Unit
}