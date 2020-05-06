package game

import agent.Agent

import scala.collection.mutable.ListBuffer

class Game(private var players: List[Agent]) {
  def play(): Unit = {
    while(players.size > 1){ //while players haven't run out of money
      val round = new Round(400, ListBuffer().addAll(players))
      println("\n\n---------------\nNew Round\n---------------\n\n")
      round.playRound()
      players = players.filter(_.getMoney > 0) //remove players who have run out of money
    }
    println(players.head.name + " wins")

  }
}
