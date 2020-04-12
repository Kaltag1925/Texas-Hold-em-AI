package game

case class Card(suit: Suite.Value, num: CardNum.Value) extends Ordered[Card] {
  override def compare(that: Card): Int = this.num.id - that.num.id
}
