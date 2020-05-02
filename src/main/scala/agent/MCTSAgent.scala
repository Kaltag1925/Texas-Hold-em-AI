package agent
import game.{Call, Card, Fold, Move, Round, WinningHand}

import util.control.Breaks._

class MCTSAgent(val name: String) extends Agent {
  private var moneyLeft = 10000
  var hand: List[Card] = null.asInstanceOf[List[Card]]

//  def getBeginningHands(): List[WinningHand] = {
//    Card.deck.diff(hand).combinations(3).map(_ ::: hand).map(WinningHand.apply).toList
//  }

//  def getAllHands(round: Round): List[WinningHand] = {
//    val knownCards = hand ::: round.getRiver()
//    val unknownCards = 7 - knownCards.size
//    val cardsLeft = Card.deck.diff(knownCards)
//    (for (i <- 0 to unknownCards) yield {
//      knownCards.combinations(knownCards.size - i).flatMap(known => cardsLeft.combinations(i)).map(WinningHand.apply).toList
//    }).flatten.toList
////
////    if (unknownCards == 2) {
////      WinningHand(knownCards) :: knownCards.combinations(4).flatMap(known => cardsLeft.map(_ :: known)).map(WinningHand.apply).toList ::: knownCards.combinations(3).flatMap(known => cardsLeft.combinations(2).map(_ ::: known)).map(WinningHand.apply).toList
////    } else {
////      knownCards.combinations(5).map(WinningHand.apply).toList ::: knownCards.combinations(4).flatMap(known => cardsLeft.map(_ :: known)).map(WinningHand.apply).toList
////    }
//  }

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
    ???
  }

  def rollout(leaf: Node): Int = ???

  def randomMove(minAmt: Int): Move = {
    if (math.random() > 0.5) {
      Fold()
    } else {
      Call(minAmt)
    }
  }

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

  override def getMove(round: Round, minAmt: Int, minimumRaise: Int): Move = ???
}
