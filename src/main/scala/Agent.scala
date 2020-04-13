

trait Agent {
  def getMove(game: Game): (Move.Value, Int);
}