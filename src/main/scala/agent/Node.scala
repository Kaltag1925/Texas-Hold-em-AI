package agent

import game.{Move, PlayerList}

class Node(r: Int, plrList: PlayerList) {
  val round = r
  var isExpanded = false
  var isVisited = false
  var parent: Node = null.asInstanceOf[Node]
  var children: List[Node] = List()
  var simulations = 0
  var wins = 0.0
  val action: Move = null
  var isTerminal = false
  var playerList = plrList
  var winner = -1
  var moveToGetHere: Move = null.asInstanceOf[Move]

  def bestUCT(): Node = {
    var bestUCTVal = -1.0
    var best = null.asInstanceOf[Node]
    for (child <- children){
      val childUCT = child.uct()
      if(childUCT > bestUCTVal){
        best = child
        bestUCTVal = childUCT
      }
    }
    best
  }

  def uct(): Double = {
    if(parent == null){
      return 0.0
    }
    if(simulations == 0){
      return Double.MaxValue
    }
    (wins/(0.0 + simulations)) + (1.41*Math.sqrt(Math.log(parent.simulations) / (0.0 + simulations)))
  }

  def printNode(): Unit = {
    println("===== Node Begin =====")
    println("round = " + round + ", wins: " + wins + ", simulations: " + simulations + ", uct: " + uct() + ", turn: " + playerList.currentTurn +
      ", isTerminal: " + isTerminal + ", winner: " + winner +
      ", moveToGetHere: " + moveToGetHere)
  }

}
