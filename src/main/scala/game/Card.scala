package game

case class Card(suit: Suite.Value, num: CardNum.Value) extends Ordered[Card] {
  override def compare(that: Card): Int = this.num.id - that.num.id
}

object Card {
  val deck: List[Card] = List( //For speed, we are using just Diamonds and Hearts
//    Card(Suite.Clubs, CardNum.Ace),
//    Card(Suite.Clubs, CardNum.Two),
//    Card(Suite.Clubs, CardNum.Three),
//    Card(Suite.Clubs, CardNum.Four),
//    Card(Suite.Clubs, CardNum.Five),
//    Card(Suite.Clubs, CardNum.Six),
//    Card(Suite.Clubs, CardNum.Seven),
//    Card(Suite.Clubs, CardNum.Eight),
//    Card(Suite.Clubs, CardNum.Nine),
//    Card(Suite.Clubs, CardNum.Ten),
//    Card(Suite.Clubs, CardNum.Jack),
//    Card(Suite.Clubs, CardNum.Queen),
//    Card(Suite.Clubs, CardNum.King),
//
//    // Spades
//
//    Card(Suite.Spades, CardNum.Ace),
//    Card(Suite.Spades, CardNum.Two),
//    Card(Suite.Spades, CardNum.Three),
//    Card(Suite.Spades, CardNum.Four),
//    Card(Suite.Spades, CardNum.Five),
//    Card(Suite.Spades, CardNum.Six),
//    Card(Suite.Spades, CardNum.Seven),
//    Card(Suite.Spades, CardNum.Eight),
//    Card(Suite.Spades, CardNum.Nine),
//    Card(Suite.Spades, CardNum.Ten),
//    Card(Suite.Spades, CardNum.Jack),
//    Card(Suite.Spades, CardNum.Queen),
//    Card(Suite.Spades, CardNum.King),
    
    // Diamonds

    Card(Suite.Diamonds, CardNum.Ace),
    Card(Suite.Diamonds, CardNum.Two),
    Card(Suite.Diamonds, CardNum.Three),
    Card(Suite.Diamonds, CardNum.Four),
    Card(Suite.Diamonds, CardNum.Five),
    Card(Suite.Diamonds, CardNum.Six),
    Card(Suite.Diamonds, CardNum.Seven),
    Card(Suite.Diamonds, CardNum.Eight),
    Card(Suite.Diamonds, CardNum.Nine),
    Card(Suite.Diamonds, CardNum.Ten),
    Card(Suite.Diamonds, CardNum.Jack),
    Card(Suite.Diamonds, CardNum.Queen),
    Card(Suite.Diamonds, CardNum.King),
    
    // Hearts

    Card(Suite.Hearts, CardNum.Ace),
    Card(Suite.Hearts, CardNum.Two),
    Card(Suite.Hearts, CardNum.Three),
    Card(Suite.Hearts, CardNum.Four),
    Card(Suite.Hearts, CardNum.Five),
    Card(Suite.Hearts, CardNum.Six),
    Card(Suite.Hearts, CardNum.Seven),
    Card(Suite.Hearts, CardNum.Eight),
    Card(Suite.Hearts, CardNum.Nine),
    Card(Suite.Hearts, CardNum.Ten),
    Card(Suite.Hearts, CardNum.Jack),
    Card(Suite.Hearts, CardNum.Queen),
    Card(Suite.Hearts, CardNum.King),
  )
}
