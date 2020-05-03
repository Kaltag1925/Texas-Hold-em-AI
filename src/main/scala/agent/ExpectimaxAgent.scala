package agent
import game.{Call, Card, Fold, Move, PlayerList, Round}

class ExpectimaxAgent(val name: String) extends Agent {
  import ExpectimaxAgent._

  val maxMoveTime = 10000

  def getUnknownCards(river: List[Card]): List[Card] = {
    Card.deck.diff(hand).diff(river)
  }

  def nextPlayerList(list: PlayerList): PlayerList = {
    val next = list.clone
    next.next
    next
  }

  override def getMove(round: Round, minAmt: Int, minimumRaise: Int): Move = {

    def buildTree(root: Node, depth: Int): Unit = {
      val unknownCards = getUnknownCards(root.river)
      val validMoves = List(Call(minAmt), Fold)
      if(root.playerList.hasNext){
        validMoves.map(m => new PlayerNode(root, List.empty, ))
        buildTree()
        val nextPlay = new PlayerNode()
      } else {
        val drawCard = new
      }
    }

    val root = new PlayerNode(null,List.empty, round.players.clone, null, round.getRiver())
    buildTree(root, 3)
  }

  override def allIn(): Boolean = ???

  override def getMoney: Int = ???

  override def hand: List[Card] = ???

  override def pay(amt: Int): Unit = ???

  override def dealHand(cards: List[Card]): Unit = ???
}

object ExpectimaxAgent{
  private abstract class Node(val parent: Node, var children: List[Node], val playerList: PlayerList, val river: List[Card]){

  }

  private class ChanceNode(parent: Node,
                           children: List[Node],
                           val odds: Double,
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
