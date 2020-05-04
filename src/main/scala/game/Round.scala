package game

import game.{Card, WinningHand}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import game.Raise
import game.Fold
import game.Check
import game.Call
import agent.Agent

class Round(blinds: Int, playersIn: ListBuffer[Agent]) {
  val players = PlayerList(playersIn.toList)

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
    playBetRound(true)

    //Flop

    cardsLeftToFlip -= 3

    while ((players.size > 1 || players.forall(_.allIn())) && cardsLeftToFlip > -1) { //TODO
      playBetRound(false)
      cardsLeftToFlip -= 1
      //println(cardsLeftToFlip)
    }

    val (winner, hand) = findWinner(players.inGame.map(p => (p, p.hand)), river)

    println(s"Winner is ${winner.name}, with a hand of ${hand.hand}")
    winner.pay(pot)
  }

  def findWinner(playerHands: List[(Agent, List[Card])], r: List[Card]): (Agent, WinningHand) = {
    val hands = playerHands.map({
      case (p, hand) => (p, findBestHand(hand, r))
    })
    hands.maxBy({
      case (_, hand) => hand
    })
  }

  private def findBestHand(hand: List[Card], r: List[Card]): WinningHand = {
    val cards = hand ++ r
    val combinations = cards.combinations(5)
    val winningHands = combinations.map(WinningHand.apply)
    winningHands.max
  }

  private def playBetRound(doBlinds: Boolean): Unit = {
    var blindsIndex = if (doBlinds) 0 else 3
    var betRequirement = if (doBlinds) blinds else 0
    var firstBet = true
    while ((firstBet || !players.isFirst) && players.size > 1) {
      blindsIndex += 1
      if (blindsIndex < 3 && doBlinds)
        betRequirement = blinds / blindsIndex //TODO
      firstBet = false
      players.currentTurn.getMove(this, betRequirement, minimumRaise) match {
        case Fold() =>
          players.remove()
          if (players.isFirst) {
            firstBet = true
          }

        case Raise(betAmt) =>
          betRequirement = betRequirement max betAmt
          players.updateStart()
          pot += betAmt
          bets(players.currentTurn) += betAmt

          players.next()

        case Check() =>
          players.next()

        case Call(betAmt) =>
          pot += betAmt
          bets(players.currentTurn) += betAmt

          players.next()
      }
    }
  }
}