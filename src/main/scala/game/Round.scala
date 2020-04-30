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
  private val river = shuffledDeck.take(5)
  shuffledDeck = shuffledDeck.drop(5)

  private var pot = 0
  private var minimumRaise = 0
  private var cardsLeftToFlip = 5
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
    var lastPlrToBet = players.head
    var currentPlayer = players.head
    var playerIndex = 0
    var blindsIndex = 0
    var betRequirement = blinds
    var firstBet = true
    while ((firstBet || currentPlayer != lastPlrToBet) && players.size > 1) {
      firstBet = false
      currentPlayer.getMove(this, betRequirement, minimumRaise) match {
        case Fold() =>
          players -= currentPlayer
          playerIndex = playerIndex % players.size
          if (currentPlayer == lastPlrToBet) {
            firstBet = true
            lastPlrToBet = players(playerIndex)
          }
          currentPlayer = players(playerIndex)
        case Raise(betAmt) =>
          betRequirement = betRequirement max betAmt
          lastPlrToBet = currentPlayer
          pot += betAmt
          bets(currentPlayer) += betAmt
          playerIndex += 1
          currentPlayer = players(playerIndex % players.size)
        case Check() =>
          playerIndex += 1
          currentPlayer = players(playerIndex % players.size)
        case Call(betAmt) =>
          blindsIndex += 1
          if (blindsIndex < 3) {
            betRequirement = blinds / blindsIndex
          } else if (blindsIndex == 3) {
            betRequirement = 0
          }
          pot += betAmt
          bets(currentPlayer) += betAmt
          playerIndex += 1
          currentPlayer = players(playerIndex % players.size)
      }
    }

    //Flop

    cardsLeftToFlip -= 3

    while ((players.size > 1 || players.forall(_.allIn())) && cardsLeftToFlip > 0) {
      playBetRound()
      cardsLeftToFlip -= 1
      //println(cardsLeftToFlip)
    }

    val hands = players.map(p => (p, findPlayerBestHand(p)))
    val (winner, hand) = hands.maxBy({
      case (_, hand) => hand
    })

    println(s"Winner is $winner, with a hand of ${hand.hand}")
    winner.pay(pot)
  }

  private def findPlayerBestHand(a: Agent): WinningHand = {
    val cards = a.hand ++ river
    val combinations = cards.combinations(5)
    val winningHands = combinations.map(WinningHand.apply)
    winningHands.max
  }

  private def playBetRound(): Unit = {
    var lastPlrToBet = players.head
    var currentPlayer = players.head
    var firstBet = true
    var playerIndex = 0
    var betRequirement = 0
    while ((firstBet || currentPlayer != lastPlrToBet) && players.size > 1) {
      firstBet = false
      currentPlayer.getMove(this, betRequirement, minimumRaise) match {
        case Fold() =>
          players -= currentPlayer
          playerIndex = playerIndex % players.size
          if (currentPlayer == lastPlrToBet) {
            firstBet = true
            lastPlrToBet = players(playerIndex)
          }
          currentPlayer = players(playerIndex)
        case Raise(betAmt) =>
          betRequirement = betRequirement max betAmt
          lastPlrToBet = currentPlayer
          pot += betAmt
          bets(currentPlayer) += betAmt
          playerIndex += 1
          currentPlayer = players(playerIndex % players.size)
        case Check() =>
          playerIndex += 1
          currentPlayer = players(playerIndex % players.size)
        case Call(betAmt) =>
          pot += betAmt
          bets(currentPlayer) += betAmt
          playerIndex += 1
          currentPlayer = players(playerIndex % players.size)
      }
    }
  }
}