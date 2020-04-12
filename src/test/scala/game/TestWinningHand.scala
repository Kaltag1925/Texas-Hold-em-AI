package game

import org.junit.Assert._
import org.junit._


class TestWinningHand {
  @Test def royalFlush: Unit = {
    val hand = List(Card(Suite.Clubs, CardNum.Ace),
      Card(Suite.Clubs, CardNum.King),
      Card(Suite.Clubs, CardNum.Queen),
      Card(Suite.Clubs, CardNum.Jack),
      Card(Suite.Clubs, CardNum.Ten))
    val handType = WinningHand(hand)
    assertTrue(handType.isInstanceOf[RoyalFlush])
  }

  @Test def straightFlush: Unit = {
    val hand = List(Card(Suite.Clubs, CardNum.Seven),
      Card(Suite.Clubs, CardNum.Six),
      Card(Suite.Clubs, CardNum.Five),
      Card(Suite.Clubs, CardNum.Four),
      Card(Suite.Clubs, CardNum.Three))
    val handType = WinningHand(hand)
    assertTrue(handType.isInstanceOf[StraightFlush])
  }

  @Test def fourKind: Unit = {
    val hand = List(Card(Suite.Clubs, CardNum.Nine),
      Card(Suite.Hearts, CardNum.Nine),
      Card(Suite.Diamonds, CardNum.Nine),
      Card(Suite.Spades, CardNum.Nine),
      Card(Suite.Clubs, CardNum.Three))
    val handType = WinningHand(hand)
    assertTrue(handType.isInstanceOf[FourKind])
  }

  @Test def fullHouse: Unit = {
    val hand = List(Card(Suite.Clubs, CardNum.Nine),
      Card(Suite.Hearts, CardNum.Nine),
      Card(Suite.Diamonds, CardNum.Nine),
      Card(Suite.Spades, CardNum.Three),
      Card(Suite.Clubs, CardNum.Three))
    val handType = WinningHand(hand)
    assertTrue(handType.isInstanceOf[FullHouse])
  }

  @Test def flush: Unit = {
    val hand = List(Card(Suite.Spades, CardNum.Ten),
      Card(Suite.Spades, CardNum.Five),
      Card(Suite.Spades, CardNum.Nine),
      Card(Suite.Spades, CardNum.Two),
      Card(Suite.Spades, CardNum.Three))
    val handType = WinningHand(hand)
    assertTrue(handType.isInstanceOf[Flush])
  }

  @Test def flushLowAce: Unit = {
    val hand = List(Card(Suite.Spades, CardNum.Ace),
      Card(Suite.Spades, CardNum.Five),
      Card(Suite.Spades, CardNum.Four),
      Card(Suite.Spades, CardNum.Three),
      Card(Suite.Spades, CardNum.Two))
    val handType = WinningHand(hand)
    assertTrue(handType.isInstanceOf[Flush])
  }

  @Test def straightHighAce: Unit = {
    val hand = List(Card(Suite.Spades, CardNum.Ace),
      Card(Suite.Hearts, CardNum.King),
      Card(Suite.Spades, CardNum.Queen),
      Card(Suite.Clubs, CardNum.Jack),
      Card(Suite.Spades, CardNum.Ten))
    val handType = WinningHand(hand)
    assertTrue(handType.isInstanceOf[Straight])
  }

  @Test def straight: Unit = {
    val hand = List(Card(Suite.Hearts, CardNum.Six),
      Card(Suite.Spades, CardNum.Five),
      Card(Suite.Clubs, CardNum.Four),
      Card(Suite.Diamonds, CardNum.Three),
      Card(Suite.Spades, CardNum.Two))
    val handType = WinningHand(hand)
    assertTrue(handType.isInstanceOf[Straight])
  }

  @Test def straightLowAce: Unit = {
    val hand = List(Card(Suite.Hearts, CardNum.Ace),
      Card(Suite.Spades, CardNum.Five),
      Card(Suite.Spades, CardNum.Four),
      Card(Suite.Diamonds, CardNum.Three),
      Card(Suite.Spades, CardNum.Two))
    val handType = WinningHand(hand)
    println(handType.getClass)
    assertTrue(handType.isInstanceOf[Straight])
  }

  @Test def threeKind: Unit = {
    val hand = List(Card(Suite.Clubs, CardNum.Nine),
      Card(Suite.Hearts, CardNum.Nine),
      Card(Suite.Diamonds, CardNum.Nine),
      Card(Suite.Spades, CardNum.Two),
      Card(Suite.Clubs, CardNum.Three))
    val handType = WinningHand(hand)
    assertTrue(handType.isInstanceOf[ThreeKind])
  }

  @Test def twoPair: Unit = {
    val hand = List(Card(Suite.Clubs, CardNum.Nine),
      Card(Suite.Hearts, CardNum.Nine),
      Card(Suite.Diamonds, CardNum.Two),
      Card(Suite.Spades, CardNum.Three),
      Card(Suite.Clubs, CardNum.Three))
    val handType = WinningHand(hand)
    assertTrue(handType.isInstanceOf[TwoPair])
  }

  @Test def onePair: Unit = {
    val hand = List(Card(Suite.Clubs, CardNum.Five),
      Card(Suite.Hearts, CardNum.Four),
      Card(Suite.Diamonds, CardNum.Two),
      Card(Suite.Spades, CardNum.Three),
      Card(Suite.Clubs, CardNum.Three))
    val handType = WinningHand(hand)
    assertTrue(handType.isInstanceOf[OnePair])
  }

  @Test def highCard: Unit = {
    val hand = List(Card(Suite.Clubs, CardNum.Jack),
      Card(Suite.Hearts, CardNum.Nine),
      Card(Suite.Diamonds, CardNum.Ace),
      Card(Suite.Spades, CardNum.Two),
      Card(Suite.Clubs, CardNum.Three))
    val handType = WinningHand(hand)
    assertTrue(handType.isInstanceOf[HighCard])
  }
}
