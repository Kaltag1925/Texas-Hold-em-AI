package agent

import game.Move

class Node() {
  var isExpanded = false
  var isVisited = false
  var parent: Node = null.asInstanceOf[Node]
  var children: List[Node] = List()
  var simulations = 0
  var wins = 0.0
  //var boardState: Nothing = null
  var turn = -1
  var isTerminal = false
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

//  def printNode(): Unit = {
//    println("===== Node Begin =====")
//
//  }

}
