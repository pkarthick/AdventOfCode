package day07

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.compiletime.ops.int
import scala.compiletime.ops.long

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(
  s"$currentDir/../../../../input/day07"
)

enum HandType:

  case HighCard
  case OnePair
  case TwoPair
  case ThreeOfAKind
  case FullHouse
  case FourOfAKind
  case FiveOfAKind

  def value() =
    this match {
      case HighCard     => 0
      case OnePair      => 1
      case TwoPair      => 2
      case ThreeOfAKind => 3
      case FullHouse    => 4
      case FourOfAKind  => 5
      case FiveOfAKind  => 6
    }

case class Hand(cards: Array[Card], bid: Int, joker: Boolean) extends Ordered[Hand]:


  override def compare(that: Hand): Int =
    val res = if joker then this.handType2.value() - that.handType2.value() else this.handType.value() - that.handType.value()
    if res != 0 then res
    else
      this.cards(0).value(this.joker) - that.cards(0).value(this.joker) match {
        case 0 =>
          this.cards(1).value(this.joker) - that.cards(1).value(this.joker) match {
            case 0 =>
              this.cards(2).value(this.joker) - that.cards(2).value(this.joker) match {
                case 0 =>
                  this.cards(3).value(this.joker) - that.cards(3).value(this.joker) match {
                    case 0 =>
                      this.cards(4).value(this.joker) - that.cards(4).value(this.joker) match {
                        case 0 => 0
                        case n => n
                      }
                    case n => n
                  }
                case n => n
              }
            case n => n
          }
        case n => n
      }

  // override def compare1(that: Hand): Int =
  //   val res = this.handType2.value() - that.handType2.value()
  //   if res != 0 then res
  //   else

  //     this.cards(0) compare that.cards(0) match {
  //       case 0 =>
  //         this.cards(1) compare that.cards(1) match {
  //           case 0 =>
  //             this.cards(2) compare that.cards(2) match {
  //               case 0 =>
  //                 this.cards(3) compare that.cards(3) match {
  //                   case 0 =>
  //                     this.cards(4) compare that.cards(4) match {
  //                       case 0 => 0
  //                       case n => n
  //                     }
  //                   case n => n
  //                 }
  //               case n => n
  //             }
  //           case n => n
  //         }
  //       case n => n
  //     }

  val m = cards.foldLeft(Map[Card, Int]()) { (m, c) =>
    m.get(c) match {
      case Some(count) => m + (c -> (count + 1))
      case None        => m + (c -> 1)
    }
  }

  val numOfJokers = m.getOrElse(Card.J, 0)

  val handType =
    m.values.toList.sorted match {
      case List(5)          => HandType.FiveOfAKind
      case List(1, 4)       => HandType.FourOfAKind
      case List(2, 3)       => HandType.FullHouse
      case List(1, 1, 3)    => HandType.ThreeOfAKind
      case List(1, 2, 2)    => HandType.TwoPair
      case List(1, 1, 1, 2) => HandType.OnePair
      case _                => HandType.HighCard
    }

  val handType2 =
    m.values.toList.sorted match {
      case List(5)                              => HandType.FiveOfAKind
      case List(1, 4) if numOfJokers == 4       => HandType.FiveOfAKind
      case List(1, 4) if numOfJokers == 1       => HandType.FiveOfAKind
      case List(1, 4)                           => HandType.FourOfAKind
      case List(2, 3) if numOfJokers == 3       => HandType.FiveOfAKind
      case List(2, 3) if numOfJokers == 2       => HandType.FiveOfAKind
      case List(2, 3)                           => HandType.FullHouse
      case List(1, 1, 3) if numOfJokers == 3    => HandType.FourOfAKind
      case List(1, 1, 3) if numOfJokers == 1    => HandType.FourOfAKind
      case List(1, 1, 3)                        => HandType.ThreeOfAKind
      case List(1, 2, 2) if numOfJokers == 2    => HandType.FourOfAKind
      case List(1, 2, 2) if numOfJokers == 1    => HandType.FullHouse
      case List(1, 2, 2)                        => HandType.TwoPair
      case List(1, 1, 1, 2) if numOfJokers == 2 => HandType.ThreeOfAKind
      case List(1, 1, 1, 2) if numOfJokers == 1 => HandType.ThreeOfAKind
      case List(1, 1, 1, 2)                     => HandType.OnePair
      case _ if numOfJokers == 1                => HandType.OnePair
      case _                                    => HandType.HighCard
    }

enum Card:

  case D(digit: Int)
  case T
  case J
  case Q
  case K
  case A

  def value(joker: Boolean) =
    this match
      case D(digit) => digit
      case T        => 10
      case J        => if joker then 1 else 11
      case Q        => 12
      case K        => 13
      case A        => 14

  def value2 =
    this match
      case J => 0
      case _ => value


def parseHands(input: String, joker: Boolean) =
  input.linesIterator.map {
    case s"$cs $bid" => {
      val cards = cs.toCharArray().map {
        case 'T' => Card.T
        case 'J' => Card.J
        case 'Q' => Card.Q
        case 'K' => Card.K
        case 'A' => Card.A
        case c   => Card.D(c.toInt - 48)
      }

      Hand(cards, bid.toInt, joker)

    }
  }.toList

def part1(input: String): String =

//   val input = """32T3K 765
// T55J5 684
// KK677 28
// KTJJT 220
// QQQJA 483"""

  val hands = parseHands(input, false)

  hands.sorted
    .zip(1 to hands.size)
    .map((h, i) => i * h.bid)
    .sum
    .toString()

def part2(input: String): String =

//   val input = """32T3K 765
// T55J5 684
// KK677 28
// KTJJT 220
// QQQJA 483"""

  val hands = parseHands(input, true)

  hands.sorted
    .zip(1 to hands.size)
    .map((h, i) => i * h.bid)
    .sum
    .toString()

@main def part12: Unit =

  val input = loadInput()
