package agent
import game.{Call, Card, Flush, Fold, FourKind, FullHouse, Move, OnePair, PlayerList, Raise, Round, RoyalFlush, Straight, StraightFlush, ThreeKind, TwoPair, WinningHand}

class ExpectimaxAgent(val name: String) extends Agent {
  import ExpectimaxAgent._

  private var moneyLeft = 1000
  var hand = null.asInstanceOf[List[Card]]

  val maxMoveTime = 10000

  def getUnknownCards(river: List[Card]): List[Card] = {
    Card.deck.diff(hand).diff(river)
  }

  def nextPlayerList(list: PlayerList): PlayerList = {
    val next = list.clone
    next.next
    next
  }

//  case class Time(start: Long, end: Long)
//
//  def averageTimes(times: List[Time]) = {
//    times.map(t => (t.end - t.start)/1e9).sum / times.length
//  }

  def scoreHand(winningHand: WinningHand, pot: Int, standToLose: Int): Double = {
    (winningHand match {
      case RoyalFlush(cards) => 25
      case StraightFlush(cards) => 16
      case FourKind(kind, extra, cards) => 9
      case FullHouse(threeKind, twoKind, cards) => 4
      case Flush(cards) => 1
      case Straight(cards) => -1
      case ThreeKind(kind, extraCards, cards) => -4
      case TwoPair(pairs, extraCards, cards) => -9
      case OnePair(pair, extraCards, cards) => -16
      case c => -25
    }) * (pot - (standToLose.toDouble))
  }

  def getNewWinningHands(oldCards: List[Card], newCards: List[Card]): List[WinningHand] = {
    (for (i <- 1 to newCards.size) yield {
        oldCards.combinations(5 - i).flatMap(cards => newCards.combinations(i).map(newC => WinningHand(newC ::: cards))).toList
    }).flatten.toList
  }

  override def getMove(round: Round, minAmt: Int, minimumRaise: Int): Move = {

    def expectimax(node: Node,  winningHands: List[WinningHand]): Double = {
      node match {
        case _: ChanceNode =>
          node.children match {
            case Nil => winningHands match {
              case Nil => 0
              case _ => winningHands.map(scoreHand(_, node.simRound.pot, node.simRound.standToLose)).max
            }

            case c =>
              val newWinningHands = winningHands match {
                case Nil => node.knownCards.combinations(5).map(WinningHand.apply).toList
                case _ => getNewWinningHands(node.parent.knownCards, node.knownCards.diff(node.parent.knownCards))
              }

              c.map(expectimax(_, newWinningHands ::: winningHands)).sum.toDouble / c.length
          }
        case node: PlayerNode =>
          if(node.parent.simRound.playerList.currentTurn != this) {
            node.children match {
              case Nil => winningHands match {
                case Nil => if (node.knownCards.length < 5) 1 else node.knownCards.combinations(5).map(WinningHand.apply).map(scoreHand(_, node.simRound.pot, node.simRound.standToLose)).max
                case _ => winningHands.map(scoreHand(_, node.simRound.pot, node.simRound.standToLose)).max
              }

              case c => c.map(expectimax(_, winningHands)).sum.toDouble / c.length
            }
          } else {
            node.children match {
              case Nil => winningHands match {
                case Nil => if (node.knownCards.length < 5) 1 else node.knownCards.combinations(5).map(WinningHand.apply).map(scoreHand(_, node.simRound.pot, node.simRound.standToLose)).max
                case _ => winningHands.map(scoreHand(_, node.simRound.pot, node.simRound.standToLose)).max
              }
              case _ => node.children.map(expectimax(_, winningHands)).max
            }
          }
      }
    }

    val maxNumBets = 2

    def buildTree(node: Node): Unit = {
      def validMoves(): List[Move] = {
        val currentPlayer = node.simRound.playerList.currentTurn
        val cash = node.simRound.moneyAmounts(currentPlayer)
        val timesBet = node.simRound.timesBet(currentPlayer)

        val raises = List(Raise(200 + minAmt)).filter(_.betAmt <= cash && timesBet < maxNumBets)
        val calls = List(Call(minAmt)).filter(_.betAmt <= cash)
        val folds = if (minAmt != 0) List(Fold()) else Nil

        calls ::: raises ::: folds
      }
      val unknownCards = getUnknownCards(node.knownCards)
      if (node.simRound.playerList.hasNext) {
        val moves = validMoves()
        val kids = moves.map(m => new PlayerNode(node, List.empty, node.simRound.simulateMove(m), m, node.knownCards))
        node.children = kids
        kids.filter(k => (k.parent.simRound.playerList.currentTurn == this && k.moveMade != Fold()) || k.parent.simRound.playerList.currentTurn != this).foreach(k => buildTree(k))
      } else {
        if (node.knownCards.size < 7 ) {
          val kids = unknownCards.map(c => new ChanceNode(node, List.empty, node.simRound.nextSet, c :: node.knownCards))
          node.children = kids
          kids.foreach(k => buildTree(k))
        }
      }
    }

    val root = new PlayerNode(null, Nil, SimRound(this, round.players.clone, round.getPot, round.getBets(this), round.players.map(p => (p, p.getMoney)).toMap, round.players.map(_ -> 0).toMap), null, round.getRiver() ::: hand)
    buildTree(root)

    try {
      val children = root.children.map(n => (n, expectimax(n, Nil)))
      val (maxChild, maxValue) = children.maxBy(_._2)
      val expectimaxVal = if (maxValue <= 0 && minAmt > 0) Fold() else maxChild.asInstanceOf[PlayerNode].moveMade
//      println("Yeet " + expectimaxVal)
//      val x = root.children.map(c => (root.simRound.playerList.currentTurn.name, expectimax(c, Nil), c.asInstanceOf[PlayerNode].moveMade))
//      x.foreach(println)
//      println(round.getRiver())
//      println(hand)

      println()
      println("----------------")
      println(s"$name does $expectimaxVal")
      expectimaxVal
    } catch {
      case e: ClassCastException => throw new Exception ("Was a chance node not a player node")
    }


  }

  override def allIn(): Boolean = moneyLeft == 0

  override def getMoney: Int = moneyLeft

  override def pay(amt: Int): Unit = moneyLeft += amt

  override def dealHand(cards: List[Card]): Unit = {
    hand = cards
  }
}

object ExpectimaxAgent{
  private abstract class Node(val parent: Node, var children: List[Node], val simRound: SimRound, val knownCards: List[Card]){

  }

  private class ChanceNode(parent: Node,
                           children: List[Node],
                           simRound: SimRound,
                           knownCards: List[Card]) extends Node(parent, children, simRound, knownCards){

  }

  private class PlayerNode(parent: Node,
                           children: List[Node],
                           simRound: SimRound,
                           val moveMade: Move,
                           knownCards: List[Card]) extends Node(parent, children, simRound, knownCards){

  }

  case class SimRound(player: Agent, playerList: PlayerList, pot: Int, standToLose: Int, moneyAmounts: Map[Agent, Int], timesBet: Map[Agent, Int]) {
    def simulateMove(move: Move): SimRound = {
      val newPlayers = playerList.clone
      val curPlayer = newPlayers.currentTurn
      move match {
        case Fold() =>
          newPlayers.remove()
          SimRound(player, newPlayers, pot, standToLose, moneyAmounts, timesBet)

        case Raise(amt) =>
          var newLoseAmt = standToLose
          if (curPlayer == player)
            newLoseAmt += amt
          newPlayers.updateStart()
          newPlayers.next()
          SimRound(player, newPlayers, pot + amt, newLoseAmt, moneyAmounts + (curPlayer -> (moneyAmounts(curPlayer) - amt)), timesBet + (curPlayer -> (timesBet(curPlayer) + 1)))

        case Call(amt) =>
          var newLoseAmt = standToLose
          if (curPlayer == player)
            newLoseAmt += amt
          newPlayers.next()
          SimRound(player, newPlayers, pot + amt, newLoseAmt, moneyAmounts + (curPlayer -> (moneyAmounts(curPlayer) - amt)), timesBet)
      }
    }

    def nextSet: SimRound = {
      val newPlayers = playerList.clone
      newPlayers.reset()
      SimRound(player, newPlayers, pot, standToLose, moneyAmounts, timesBet)
    }
  }
}
