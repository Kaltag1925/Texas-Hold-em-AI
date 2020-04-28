package game

import scala.annotation.tailrec

object WinningHand {
  def apply(hand: List[Card]): WinningHand = {
    val sorted = hand.sorted.reverse
    val counts = hand.groupBy(_.num).view.mapValues(_.size).toMap
    if (counts.size < 5) {
      // Not a flush since there is at least a pair
      val maxComboSize = counts.values.max
      if (maxComboSize == 4) {
        val swapped = counts.map(_.swap)
        FourKind(swapped(4), swapped(1), sorted)

      } else if (maxComboSize == 3) {
        if (counts.size == 2) {
          // 2 sets a 3x and 2x
          val swapped = counts.map(_.swap)
          FullHouse(swapped(3), swapped(2), sorted)

        } else {
          // 3 sets 3x 1x and 1x
          val others = counts.filter(_._2 == 1).keys.toList.sorted.reverse
          val swapped = counts.map(_.swap)
          ThreeKind(swapped(3), others, sorted)
        }

      } else { // Max combo is 2
        if (counts.size == 3) {
          // 3 sets 2x 2x 1x
          val pairs = counts.filter(_._2 == 2).keys.toList.sorted.reverse
          val swapped = counts.map(_.swap)
          TwoPair(pairs, List(swapped(1)), sorted)
        } else {
          // 4 sets 2x 1x 1x 1x
          val others = counts.filter(_._2 == 1).keys.toList.sorted.reverse
          val swapped = counts.map(_.swap)
          OnePair(swapped(2), others, sorted)
        }
      }
    } else {
      val orderStatus = inOrder(sorted.map(_.num))
      if (orderStatus == AllLegal) {
        if (sameSuit(sorted)) {
          if (sorted.head.num == CardNum.Ace) {
            RoyalFlush(hand) // Congratulations
          } else {
            StraightFlush(hand)
          }
        } else {
          Straight(hand)
        }
      } else if (orderStatus == StraightLegal) {
        if (sameSuit(sorted)) {
          Flush(sorted)
        } else {
          Straight(sorted)
        }
      } else {
        // Not in perfect order
        if (sameSuit(sorted)) {
          Flush(sorted)
        } else {
          HighCard(sorted)
        }
      }
    }

  }

  def sameSuit(cards: List[Card]): Boolean = {
    val suit = cards.head.suit
    cards.forall(_.suit == suit)
  }

  sealed trait OrderStatus
  case object StraightLegal extends OrderStatus
  case object AllLegal extends OrderStatus
  case object NotInOrder extends OrderStatus

  @tailrec
  def inOrder(cards: List[CardNum.Value]): OrderStatus = {
    cards match {
      case CardNum.Ace :: CardNum.Five :: CardNum.Four :: CardNum.Three :: CardNum.Two :: Nil => StraightLegal
      case h :: (t@(next :: _)) =>
        if (h.id - 1 == next.id) {
          inOrder(t)
        } else {
          NotInOrder
        }
      case h :: Nil => AllLegal
      case Nil => AllLegal
    }
  }
  @tailrec
  def compareFirstDifferentCard(c1: List[CardNum.Value], c2:List[CardNum.Value]): Int = {
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

abstract class WinningHand(val rank: Int, val hand: List[Card], val compareHand: List[CardNum.Value]) extends Ordered[WinningHand] {
  def compare(that: WinningHand): Int = {
    if (this.getClass == that.getClass) {
      WinningHand.compareFirstDifferentCard(this.compareHand, that.compareHand)
    } else {
      this.rank - that.rank
    }
  }
}

case class HighCard(cards: List[Card])
  extends WinningHand(0, cards, cards.map(_.num))

case class OnePair(pair: CardNum.Value, extraCards: List[CardNum.Value], cards: List[Card])
  extends WinningHand(1, cards, (pair :: extraCards))

case class TwoPair(pairs: List[CardNum.Value], extraCards: List[CardNum.Value], cards: List[Card])
  extends WinningHand(2, cards, (pairs ::: extraCards))

case class ThreeKind(kind: CardNum.Value, extraCards: List[CardNum.Value], cards: List[Card])
  extends WinningHand(3, cards, (kind :: extraCards))

case class Straight(cards: List[Card])
  extends WinningHand(4, cards, List(cards.max.num))

case class Flush(cards: List[Card])
  extends WinningHand(5, cards, cards.map(_.num))

case class FullHouse(threeKind: CardNum.Value, twoKind: CardNum.Value, cards: List[Card])
  extends WinningHand(6, cards, List(threeKind, twoKind))

case class FourKind(kind: CardNum.Value, extra: CardNum.Value, cards: List[Card])
  extends WinningHand(7, cards, List(kind, extra))

case class StraightFlush(cards: List[Card])
  extends WinningHand(8, cards, List(cards.max.num))

case class RoyalFlush(cards: List[Card])
  extends WinningHand(9, cards, Nil) {
}