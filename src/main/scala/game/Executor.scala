package game

import scala.collection.mutable.ListBuffer
import agent.{Agent, ExpectimaxAgent, Human}
import game.Round

object Executor extends App {
  private def runGame(a1: Agent, a2: Agent): Unit = {
    val game = new Game(List(a1, a2))
    game.play()
  }
  
  

  runGame(new ExpectimaxAgent("Player 1"), new Human("Player 2"))
  
  
}