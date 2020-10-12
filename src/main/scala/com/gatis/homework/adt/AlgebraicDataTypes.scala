package com.gatis.homework.adt

import com.gatis.homework.adt.AlgebraicDataTypes.Hand._

object AlgebraicDataTypes {

  // As per the homework task, this is not a complete implementation of the pre-bootcamp task, but
  // a model of ADTs needed for such a solution. As such, some implemetation-specific detail
  // is missing, described in comments.

  //Suit
  sealed trait Suit
  object Suit {
    final case object Spade extends Suit
    final case object Club extends Suit
    final case object Heart extends Suit
    final case object Diamond extends Suit
    def of(value: String): Option[Suit] = {
      value match {
        case "s" => Some(Spade)
        case "c" => Some(Club)
        case "h" => Some(Heart)
        case "d" => Some(Diamond)
        case _   => None
      }
    }
  }

  //Rank
  sealed trait Rank extends Comparable[Rank] {
    val value: Int
    override def compareTo(other: Rank): Int = value - other.value
  }
  object Rank {
    final case class Two(value: Int = 0) extends Rank
    final case class Three(value: Int = 1) extends Rank
    final case class Four(value: Int = 2) extends Rank
    final case class Five(value: Int = 3) extends Rank
    final case class Six(value: Int = 4) extends Rank
    final case class Seven(value: Int = 5) extends Rank
    final case class Eight(value: Int = 6) extends Rank
    final case class Nine(value: Int = 7) extends Rank
    final case class Ten(value: Int = 8) extends Rank
    final case class Jack(value: Int = 9) extends Rank
    final case class Queen(value: Int = 10) extends Rank
    final case class King(value: Int = 11) extends Rank
    final case class Ace(value: Int = 12) extends Rank
    def of(value: String): Option[Rank] = {
      value match {
        case "2" => Some(Two())
        case "3" => Some(Three())
        case "4" => Some(Four())
        case "5" => Some(Five())
        case "6" => Some(Six())
        case "7" => Some(Seven())
        case "8" => Some(Eight())
        case "9" => Some(Nine())
        case "T" => Some(Ten())
        case "J" => Some(Jack())
        case "Q" => Some(Queen())
        case "K" => Some(King())
        case "A" => Some(Ace())
        case _   => None
      }
    }
  }

  //Card
  final case class Card(suit: Suit, rank: Rank) extends Comparable[Card] {
    override def compareTo(other: Card): Int = rank.compareTo(other.rank)
  }
  object Card {
    def of(suit: String, rank: String): Option[Card] = (Suit.of(suit), Rank.of(rank)) match {
      case (Some(suit: Suit), Some(rank: Rank)) => Some(Card(suit, rank))
      case _                                    => None
    }
    def getCardsFromString(cards: String): Option[List[Card]] =
      //convert string into card list
      ???
  }

  //Hand
  sealed trait Hand {
    val cards: List[Card]
  }
  object Hand {
    final case class HoldemHand(cards: List[Card]) extends Hand
    final case class OmahaHand(cards: List[Card]) extends Hand
    def of(cards: String): Option[Hand] = cards.length match {
      case 4 | 8 =>
        Card.getCardsFromString(cards) match {
          case Some(cards) =>
            cards.length match {
              case 4 => Some(HoldemHand(cards))
              case _ => Some(OmahaHand(cards))
            }
          case _ => None
        }
      case _ => None
    }
  }

  // Board
  final case class Board(boardCards: List[Card])
  object Board {
    def of(boardCards: String): Option[Board] = {
      if (boardCards.length == 10) {
        Card.getCardsFromString(boardCards) match {
          case Some(cards) => Some(Board(cards))
          case None        => None
        }
      } else None
    }
  }

  // Poker Combination
  sealed trait PokerCombination extends Comparable[PokerCombination] {
    val value: Int
    override def compareTo(other: PokerCombination): Int = {
      val handStrengthTypeComparison = value - other.value
      if (handStrengthTypeComparison == 0) compareToEqualStrengthType(other) else handStrengthTypeComparison
    }
    def compareToEqualStrengthType(other: PokerCombination): Int
  }
  object PokerCombination {
    final case class HighCard(value: Int = 0, kickers: List[Rank]) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    final case class Pair(value: Int = 1, pairRank: Rank, kickers: List[Rank]) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    final case class TwoPairs(value: Int = 2, pairsRanks: List[Rank], kicker: Rank) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    final case class ThreeOfAKind(value: Int = 3, three: Rank, kickers: List[Rank]) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    final case class Straight(value: Int = 4, highestRank: Rank) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    final case class Flush(value: Int = 5, ranks: List[Rank]) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    final case class FullHouse(value: Int = 6, three: Rank, pair: Rank) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    final case class FourOfAKind(value: Int = 7, four: Rank, kicker: Rank) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    final case class StraightFlush(value: Int = 8, highestRank: Rank) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    def of(hand: Hand, board: Board): PokerCombination =
      // determine PokerCombination
      hand match {
        case HoldemHand(cards) => ???
        case OmahaHand(cards)  => ???
      }
  }

  // Test Case
  final case class TestCase(boardCards: Board, hands: List[Hand])
  object TestCase {
    //parse a deal (one line of input as for the pre-bootcamp task)
    def of(deal: String): Option[TestCase] = ???
  }

  // Test Result
  final case class TestResult(sortedHandList: List[Set[Hand]])
  object TestResult {
    //obtain list of hands ordered by ranks, accounting for draws in strength
    def of(evaluatedHands: List[(Hand, PokerCombination)]): TestResult = ???
  }
}
