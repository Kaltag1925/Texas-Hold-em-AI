import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Executor extends App {
  
  private def runGame(a1: Agent, a2: Agent): Unit = {
    val round = new Round(400, ListBuffer(a1, a2));
    round.playRound()

  }
  
  
  
  runGame(new Human(), new Human())
  
  
}