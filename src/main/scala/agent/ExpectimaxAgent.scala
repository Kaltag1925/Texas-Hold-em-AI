package agent
import game.{Call, Card, Fold, Move, PlayerList, Round, WinningHand}

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

  def scoreHand(node: Node): Int = ???

  override def getMove(round: Round, minAmt: Int, minimumRaise: Int): Move = {

    def expectimax(node: Node): Double = {

      if(node.playerList.currentTurn != this) {
        node.children match {
          case Nil => scoreHand(node)
          case c => c.map(expectimax).sum.toDouble / c.length
        }
      } else {
        node.children.map(expectimax).max
      }
    }

    def buildTree(root: Node, depth: Int): Unit = {

        val unknownCards = getUnknownCards(root.river)
        val validMoves = List(Call(minAmt), Fold())
        if (root.playerList.hasNext) {
          val kids = validMoves.map(m => new PlayerNode(root, List.empty, nextPlayerList(root.playerList), m, root.river))
          root.children = kids
          kids.foreach(k => buildTree(k, depth))
        } else {
          if (depth > 0) {
            val kids = unknownCards.map(c => new ChanceNode(root, List.empty, nextPlayerList(root.playerList), c :: root.river))
            root.children = kids
            kids.foreach(k => buildTree(k, depth - 1))
          }
          else {

          }
        }
    }

    val root = new PlayerNode(null,List.empty, round.players.clone, null, round.getRiver())
    buildTree(root, 3)

    try {
      root.children.maxBy(expectimax).asInstanceOf[PlayerNode].moveMade
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
  private abstract class Node(val parent: Node, var children: List[Node], val playerList: PlayerList, val river: List[Card]){

  }

  private class ChanceNode(parent: Node,
                           children: List[Node],
                           playerList: PlayerList,
                           river: List[Card]) extends Node(parent, children, playerList, river){

  }

  private class PlayerNode(parent: Node,
                           children: List[Node],
                           player: PlayerList,
                           val moveMade: Move,
                           river: List[Card]) extends Node(parent, children, player, river){

  }

}
