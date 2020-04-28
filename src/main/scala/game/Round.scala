package game

import game.{Card, WinningHand}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import game.Raise
import game.Fold
import game.Check
import game.Call
import agent.Agent

class Round(blinds: Int, private var players: ListBuffer[Agent]) {

  private var shuffledDeck = util.Random.shuffle(Card.deck)
  private val river = shuffledDeck.take(3)
  shuffledDeck = shuffledDeck.drop(3)

  private var pot = 0
  private var minimumRaise = 0
  private var cardsLeftToFlip = 3
  private val bets = mutable.Map.from(players.map(a => (a, 0)))

  def getRiver(): List[Card] = {
    river.drop(cardsLeftToFlip)
  }

  def divideCards(player: Agent): Unit = {
    player.dealHand(shuffledDeck.take(2))
    shuffledDeck = shuffledDeck.drop(2)
  }

  def playRound(): Unit = {
    players.foreach(divideCards)
    //BlindsRound
    var lastPlrToBet = players(0)
    var currentPlayer = players(1)
    var playerIndex = 1
    var blindsIndex = 1
    var betRequirement = blinds
    while (currentPlayer != lastPlrToBet) {
      currentPlayer.getMove(this, betRequirement, minimumRaise) match {
        case Fold() =>
          players -= currentPlayer
        case Raise(betAmt) =>
          betRequirement = betRequirement max betAmt
          lastPlrToBet = currentPlayer
          pot += betAmt
          bets(currentPlayer) += betAmt
          currentPlayer = players((playerIndex + 1) % players.size)
        case Check() =>
          currentPlayer = players((playerIndex + 1) % players.size)
        case Call(betAmt) =>
          blindsIndex += 1
          if (blindsIndex == 2) {
            betRequirement = blinds / 2
          } else if (blindsIndex == 3) {
            betRequirement = 0
          }
          pot += betAmt
          bets(currentPlayer) += betAmt
          currentPlayer = players((playerIndex + 1) % players.size)
      }

      //Flop

      cardsLeftToFlip -= 1
      river.drop(cardsLeftToFlip).foreach(println)

      while (players.size > 1 || players.forall(_.allIn()) || cardsLeftToFlip <= 0) {
        playBetRound()
        cardsLeftToFlip -= 1
        river.drop(cardsLeftToFlip).foreach(println)
      }

      val hands = players.map(a => (a, WinningHand(a.hand ++ river)))
      val winner = hands.maxBy({
        case (_, hand) => hand
      })._1

      winner.pay(pot)

    }
  }

  def playBetRound(): Unit = {
    var lastPlrToBet = players(0)
    var currentPlayer = players(1)
    var playerIndex = 1
    var betRequirement = 0
    while (currentPlayer != lastPlrToBet) {
      currentPlayer.getMove(this, betRequirement, minimumRaise) match {
        case Fold() =>
          players -= currentPlayer
        case Raise(betAmt) =>
          betRequirement = betRequirement max betAmt
          lastPlrToBet = currentPlayer
          pot += betAmt
          bets(currentPlayer) += betAmt
          currentPlayer = players((playerIndex + 1) % players.size)
        case Check() =>
          currentPlayer = players((playerIndex + 1) % players.size)
        case Call(betAmt) =>
          pot += betAmt
          bets(currentPlayer) += betAmt
          currentPlayer = players((playerIndex + 1) % players.size)
      }
    }
  }
}