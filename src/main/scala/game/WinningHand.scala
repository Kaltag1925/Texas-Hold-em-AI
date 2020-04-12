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
        FourKind(swapped(4), swapped(1))

      } else if (maxComboSize == 3) {
        if (counts.size == 2) {
          // 2 sets a 3x and 2x
          val swapped = counts.map(_.swap)
          FullHouse(swapped(3), swapped(2))

        } else {
          // 3 sets 3x 1x and 1x
          val others = counts.filter(_._2 == 1).keys.toList.sorted.reverse
          val swapped = counts.map(_.swap)
          ThreeKind(swapped(3), others)
        }

      } else { // Max combo is 2
        if (counts.size == 3) {
          // 3 sets 2x 2x 1x
          val pairs = counts.filter(_._2 == 2).keys.toList.sorted.reverse
          val swapped = counts.map(_.swap)
          TwoPair(pairs, List(swapped(1)))
        } else {
          // 4 sets 2x 1x 1x 1x
          val others = counts.filter(_._2 == 1).keys.toList.sorted.reverse
          val swapped = counts.map(_.swap)
          OnePair(swapped(2), others)
        }
      }
    } else {
      val orderStatus = inOrder(sorted.map(_.num))
      if (orderStatus == AllLegal) {
        if (sameSuit(sorted)) {
          if (sorted.head.num == CardNum.Ace) {
            RoyalFlush() // Congratulations
          } else {
            StraightFlush(sorted.head.num)
          }
        } else {
          Straight(sorted.head.num)
        }
      } else if (orderStatus == StraightLegal) {
        if (sameSuit(sorted)) {
          Flush(sorted.map(_.num))
        } else {
          Straight(sorted.head.num)
        }
      } else {
        // Not in perfect order
        if (sameSuit(sorted)) {
          Flush(sorted.map(_.num))
        } else {
          HighCard(sorted.map(_.num))
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

abstract class WinningHand(val rank: Int) extends Ordered[WinningHand] {
  def compare(that: WinningHand): Int = this.rank - that.rank
}

case class HighCard(cards: List[CardNum.Value]) extends WinningHand(0){
  override def compare(that: WinningHand): Int = {
    that match {
      case HighCard(thatCards) => WinningHand.compareFirstDifferentCard(cards, thatCards)
      case other => super.compare(other)
    }
  }
}

case class OnePair(pair: CardNum.Value, cards: List[CardNum.Value]) extends WinningHand(1) {
  override def compare(that: WinningHand): Int = {
    that match {
      case OnePair(thatPair, thatCards) =>
        if (pair == thatPair) {
          WinningHand.compareFirstDifferentCard(cards, thatCards)
        } else {
          pair.compare(thatPair)
        }
      case other => super.compare(other)
    }
  }
}

case class TwoPair(pairs: List[CardNum.Value], cards: List[CardNum.Value]) extends WinningHand(2) {
  override def compare(that: WinningHand): Int = {
    that match {
      case TwoPair(thatPairs, thatCards) =>
        val pairCompare = WinningHand.compareFirstDifferentCard(pairs, thatPairs)// I wanted to write it as pairComPAIR
        if (pairCompare == 0) {
          WinningHand.compareFirstDifferentCard(cards, thatCards)
        } else {
          pairCompare
        }
      case other => super.compare(other)
    }
  }
}

case class ThreeKind(kind: CardNum.Value, cards: List[CardNum.Value]) extends WinningHand(3) {
  override def compare(that: WinningHand): Int = {
    that match {
      case ThreeKind(thatKind, thatCards) =>
        if (kind == thatKind) {
          WinningHand.compareFirstDifferentCard(cards, thatCards)
        } else {
          kind.compare(thatKind)
        }
      case other => super.compare(other)
    }
  }
}

case class Straight(highestCard: CardNum.Value) extends WinningHand(4) {
  override def compare(that: WinningHand): Int = {
    that match {
      case Straight(thatHighestCard) => highestCard.compare(thatHighestCard)
      case other => super.compare(other)
    }
  }
}

case class Flush(cards: List[CardNum.Value]) extends WinningHand(5) {
  override def compare(that: WinningHand): Int = {
    that match {
      case Flush(thatCards) =>
        WinningHand.compareFirstDifferentCard(cards, thatCards)
      case other => super.compare(other)
    }
  }
}

case class FullHouse(threeKind: CardNum.Value, twoKind: CardNum.Value) extends WinningHand(6) {
  override def compare(that: WinningHand): Int = {
    that match {
      case FullHouse(thatThreeKind, thatTwoKind) =>
        if (threeKind == thatThreeKind) {
          twoKind.compare(thatTwoKind)
        } else {
          threeKind.compare(thatThreeKind)
        }
      case other => super.compare(other)
    }
  }
}

case class FourKind(kind: CardNum.Value, extra: CardNum.Value) extends WinningHand(7) {
  override def compare(that: WinningHand): Int = {
    that match {
      case FourKind(thatKind, thatExtra) =>
        if (kind == thatKind) {
          extra.compare(thatExtra)
        } else {
          kind.compare(thatKind)
        }
      case other => super.compare(other)
    }
  }
}

case class StraightFlush(highestCard: CardNum.Value) extends WinningHand(8) {
  override def compare(that: WinningHand): Int = {
    that match {
      case StraightFlush(thatHighestCard) => highestCard.compare(thatHighestCard)
      case other => super.compare(other)
    }
  }
}

case class RoyalFlush() extends WinningHand(9) {
  override def compare(that: WinningHand): Int = {
    that match {
      case _: RoyalFlush => 0
      case other => super.compare(other)
    }
  }
}