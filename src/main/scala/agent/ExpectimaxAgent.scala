package agent
import game.{Call, Card, Flush, Fold, FourKind, FullHouse, Move, OnePair, PlayerList, Round, RoyalFlush, Straight, StraightFlush, ThreeKind, TwoPair, WinningHand}

class ExpectimaxAgent(val name: String) extends Agent {
  import ExpectimaxAgent._

  private var moneyLeft = 10000
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

  case class Time(start: Long, end: Long)

  var handTimes: List[Time] = Nil
  var expectiTimes: List[Time] = Nil
  var oldSizes: List[Int] = Nil
  var newSizes: List[Int] = Nil

  def averageTimes(times: List[Time]) = {
    times.map(t => (t.end - t.start)/1e9).sum / times.length
  }

  def scoreHand(winningHand: WinningHand): Int = winningHand match {
    case RoyalFlush(cards) => 100
    case StraightFlush(cards) => 90
    case FourKind(kind, extra, cards) => 80
    case FullHouse(threeKind, twoKind, cards) => 70
    case Flush(cards) => 60
    case Straight(cards) => 30
    case ThreeKind(kind, extraCards, cards) => 10
    case TwoPair(pairs, extraCards, cards) => 5
    case OnePair(pair, extraCards, cards) => 1
    case c => 0
  }

  def getNewWinningHands(oldCards: List[Card], newCards: List[Card]): List[WinningHand] = {
    val start = System.nanoTime()

    oldSizes ::= oldCards.size
    newSizes ::= newCards.size
    try {
      val ret = (for (i <- 1 to newCards.size) yield {
        oldCards.combinations(5 - i).flatMap(cards => newCards.combinations(i).map(newC => WinningHand(newC ::: cards))).toList
      }).flatten.toList
      handTimes ::= Time(start, System.nanoTime())

      ret
    } catch {
      case e: NoSuchElementException => throw new Exception(oldCards.size.toString + " " + newCards.length.toString)
    }
  }

  override def getMove(round: Round, minAmt: Int, minimumRaise: Int): Move = {

    var iters = 0
    def expectimax(node: Node,  winningHands: List[WinningHand]): Double = {
      iters += 1
      node match {
        case _: ChanceNode =>
          node.children match {
            case Nil => winningHands.map(scoreHand).max
            case c =>
              val newWinningHands = winningHands match {
                case Nil => node.knownCards.combinations(5).map(WinningHand.apply).toList
                case _ => getNewWinningHands(node.parent.knownCards, node.knownCards.diff(node.parent.knownCards))
              }

              if (winningHands.diff(newWinningHands).size != winningHands.size)
                println(false)
              c.map(expectimax(_, newWinningHands ::: winningHands)).sum.toDouble / c.length
          }
        case _: PlayerNode =>
          if(node.playerList.currentTurn != this) {
            node.children match {
              case Nil => winningHands match {
                case Nil => node.knownCards.combinations(5).map(WinningHand.apply).map(scoreHand).max
                case _ => winningHands.map(scoreHand).max
              }

              case c => c.map(expectimax(_, winningHands)).sum.toDouble / c.length
            }
          } else {
            node.children.map(expectimax(_, winningHands)).max
          }
      }
    }

    var num = 0
    def buildTree(root: Node, depth: Int): Unit = {
      num += 1
      val unknownCards = getUnknownCards(root.knownCards)
      val validMoves = List(Call(minAmt), Fold())
      if (root.playerList.hasNext) {
        val kids = validMoves.map(m => new PlayerNode(root, List.empty, nextPlayerList(root.playerList), m, root.knownCards))
        root.children = kids
        kids.foreach(k => buildTree(k, depth))
      } else {
        if (depth > 0 && root.knownCards.size < 7 ) {
          val kids = unknownCards.map(c => new ChanceNode(root, List.empty, nextPlayerList(root.playerList), c :: root.knownCards))
          root.children = kids
          kids.foreach(k => buildTree(k, depth - 1))
        }
      }
    }

    val root = new PlayerNode(null, Nil, round.players.clone, null, round.getRiver() ::: hand)
    buildTree(root, 3)

    println(num)

    try {
      val expectimaxVal = root.children.maxBy(expectimax(_, Nil)).asInstanceOf[PlayerNode].moveMade
      println(expectimaxVal)
      println(averageTimes(handTimes))
      println(iters)
      println(newSizes.distinct)
      println(oldSizes.distinct)
      newSizes = Nil
      oldSizes = Nil
      handTimes = Nil
      println(round.getRiver())
      println(hand)
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
  private abstract class Node(val parent: Node, var children: List[Node], val playerList: PlayerList, val knownCards: List[Card]){

  }

  private class ChanceNode(parent: Node,
                           children: List[Node],
                           playerList: PlayerList,
                           knownCards: List[Card]) extends Node(parent, children, playerList, knownCards){

  }

  private class PlayerNode(parent: Node,
                           children: List[Node],
                           player: PlayerList,
                           val moveMade: Move,
                           knownCards: List[Card]) extends Node(parent, children, player, knownCards){

  }

}
