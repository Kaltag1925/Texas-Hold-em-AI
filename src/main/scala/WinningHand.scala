import scala.annotation.tailrec

protected object WinningHand {
  @tailrec
  def compareFirstDifferentCard(c1: List[Card], c2:List[Card]): Int = {
    (c1, c2) match {
      case (Nil, Nil) => 0
      case (h1 :: t1, h2 :: t2) =>
        if (h1.num == h2.num) {
          return h1.compare(h2)
        } else {
          compareFirstDifferentCard(t1, t2)
        }
      /*
      These should never happen
      case (Nil, h :: t) =>
      case (h :: t, Nil) =>
       */
    }
  }
}

sealed abstract class WinningHand(rank: Int) extends Ordered[WinningHand] {
  def compare(that: WinningHand): Int = this.rank - that.rank
}

case class HighCard(cards: List[Card]) extends WinningHand(0){
  override def compare(that: WinningHand): Int = {
    that match {
      case HighCard(thatCards) => WinningHand.compareFirstDifferentCard(cards, thatCards)
      case other => super.compare(other)
    }
  }
}

case class OnePair(pair: Card, cards: List[Card]) extends WinningHand(1) {
  override def compare(that: WinningHand): Int = {
    that match {
      case OnePair(thatPair, thatCards) =>
        if (pair.num == thatPair.num) {
          WinningHand.compareFirstDifferentCard(cards, thatCards)
        } else {
          pair.compare(thatPair)
        }
      case other => super.compare(other)
    }
  }
}

case class TwoPair(pairs: List[Card], cards)