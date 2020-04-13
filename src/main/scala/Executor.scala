

object Executor extends App {
  
  private def runGame(a1: Agent, a2: Agent): Unit = {
    val game = new Game(a1, a2);
    
    while(!game.gameOver){
      if(game.getTurn == 1){
        val (mv, amt) = a1.getMove(game)
        game.makeMove(a1, mv, amt)
      } else {
        val (mv, amt) = a2.getMove(game)
        game.makeMove(a2, mv, amt)
      }
    }
  }
  
  
  
  runGame(new Human(), new Human())
  
  
}