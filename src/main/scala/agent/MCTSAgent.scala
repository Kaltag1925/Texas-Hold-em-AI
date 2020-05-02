package agent
import game.{Card, Move, Round}
import util.control.Breaks._

class MCTSAgent(val name: String) extends Agent {
  private var moneyLeft = 10000
  var hand: List[Card] = null.asInstanceOf[List[Card]]

  def getAllHands(): List[List[Card]] = {
    Card.deck.diff(hand).combinations(3).map(_ ::: hand).toList
  }

  def getOdds(): Unit = ???

  def getMove(round: Round, minAmt: Int, minimumRaise: Int, timeLimit: Long): Move = ???


  def doMCTS(root: Node, timeLimit: Long): Unit = {
    val startTime = System.currentTimeMillis()
    val timeDue = startTime + timeLimit
    var node = root
    var iterCount = 0
    while(System.currentTimeMillis() < timeDue && iterCount < 1000000){
      node = selection(root)
      val rolloutRes = rollout(node)
      backpropagate(node, rolloutRes)
      if(node.parent != null && !node.parent.isExpanded){
        checkNodeExpansion(node.parent)
      }
    }
  }

  def selection(node: Node): Node = {
    var rover = node
    while(node.isExpanded && !node.isTerminal){
      rover = rover.bestUCT()
    }

    if(!rover.isVisited){
      if(!rover.isTerminal){
        addChildren(rover)
      }
      node.isVisited = true
    }

    if(rover.isTerminal){
      return rover
    }
    for(child <- rover.children) {
      breakable {
        if (child.isVisited) {
          break()
        } else {
          if(!child.isTerminal){
            addChildren(child)
          }
          return child
        }
      }
    }
  }

  def rollout(leaf: Node): Int = ???

  def backpropagate(node: Node, result: Int): Unit = ??? //turns

  def addChildren(node: Node): Unit = ???

  def checkNodeExpansion(node: Node): Unit = {
    if(node.children.isEmpty){
      return
    }
    for (child <- node.children){
      if(!child.isVisited){
        return
      }
    }
    node.isExpanded = true
  }

  def allIn(): Boolean = moneyLeft == 0

  def getMoney: Int = moneyLeft

  override def pay(amt: Int): Unit = moneyLeft += amt

  override def dealHand(cards: List[Card]): Unit = {
    hand = cards
  }
}
