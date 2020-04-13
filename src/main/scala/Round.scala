import game.Card

class Round(river: List[Card]) {
  private var pot = 0
  private var minimumRaise = 0
  private var cardsLeftToFlip = 3
  
  
  def playRound(){
	  var lastPlrToBet = 0
	  var currPlr = 1
    while(lastPlrToBet != currPlr){
      currPlr
    }
  }
  
  def makeMove(player: Agent, mv: Move.Value, amt: Int) {
    if (mv == Move.Bet) {
      minimumRaise = amt
      pot += amt
    }
    turn *= -1
  }
}