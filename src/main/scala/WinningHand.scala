import scala.annotation.tailrec

sealed abstract class WinningHand(rank: Int) extends Ordering[WinningHand] {
  override def (that: WinningHand): Int = this.rank - that.rank
}

case class HighCard(cards: List[Card]) extends WinningHand(1) with Ordering[HighCard] {
  def compare(that: HighCard): Int = compareFirstDifferentCard(cards, that.cards)

  @tailrec
  private def compareFirstDifferentCard(c1: List[Card], c2:List[Card]): Int = {
    (c1, c2) match {
      case (Nil, Nil) => 0
      case (h1 :: t1, h2 :: t2) =>
        if (h1 == h2) {
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