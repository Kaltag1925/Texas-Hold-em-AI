case class Card(suite: Suite.Value, num: CardNum.Value) extends Ordered[Card] {
  override def compare(that: Card): Int = this.num.id - that.num.id
}

object Card {
  type CardList = List[Card]
  lazy val fullDeck = null

  def loadCards(): CardList = {
    return
  }
}