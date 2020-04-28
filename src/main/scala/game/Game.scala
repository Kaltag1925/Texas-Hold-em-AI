package game

import agent.Agent

import scala.collection.mutable.ListBuffer

class Game(val players: List[Agent]) {
  def play(): Unit = {
    val round = new Round(400, ListBuffer().addAll(players))
    round.playRound()
  }
}
