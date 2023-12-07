//> using scala "3.3.1"
//> using jvm zulu:17

def readInput(): Seq[String] = {
  val source = scala.io.Source.fromFile("input.txt")
  val input = source.getLines().toSeq
  source.close()
  input
}

enum HandType(val value: Int) {
  case FiveOfAKind  extends HandType(7)
  case FourOfAKind  extends HandType(6)
  case FullHouse    extends HandType(5)
  case ThreeOfAKind extends HandType(4)
  case TwoPair      extends HandType(3)
  case OnePair      extends HandType(2)
  case HighCard     extends HandType(1)
}

object HandType {
  private def fromPairs(pairs: Seq[(Char, Int)]): HandType = {
    pairs.size match {
      case 1                       => HandType.FiveOfAKind
      case 2 if pairs.head._2 == 4 => HandType.FourOfAKind
      case 2 if pairs.head._2 == 3 => HandType.FullHouse
      case 3 if pairs.head._2 == 3 => HandType.ThreeOfAKind
      case 3 if pairs.head._2 == 2 => HandType.TwoPair
      case 4                       => HandType.OnePair
      case _                       => HandType.HighCard
    }
  }

  def determine(cards: String): HandType = {
    val kinds = cards.view.drop(0)
      .foldLeft(Seq.empty[(Char, Int)]) {
      case (kinds, card) =>
        kinds.indexWhere(_._1 == card) match {
          case -1 =>
            kinds :+ (card, 1)
          case index =>
            val (c, n) = kinds(index)
            kinds.updated(index, (c, n + 1))
        }
    }
    //println(kinds)
    val sorted = kinds.sortBy(_._2).reverse
    fromPairs(sorted)
  }

  def determine2(cards: String): HandType = {
    val kinds = cards.view.drop(0)
      .foldLeft(Seq.empty[(Char, Int)]) {
      case (kinds, card) =>
        kinds.indexWhere(_._1 == card) match {
          case -1 =>
            kinds :+ (card, 1)
          case index =>
            val (c, n) = kinds(index)
            kinds.updated(index, (c, n + 1))
        }
    }
    //println(kinds)
    val sorted = kinds.sortBy(_._2).reverse
    val newKinds = sorted.find(_._1 == 'J') match {
      case Some((_, jokersN)) if jokersN < 5 =>
        val kindsWoJokers = sorted.filter(_._1 != 'J')
        val (firstKindCard, firstKindN) = kindsWoJokers.head
        (firstKindCard, firstKindN + jokersN) +: kindsWoJokers.tail
      case Some((_, _)) =>
        // All jokers
        sorted
      case None => 
        sorted
    }
    //println(newKinds)
    fromPairs(newKinds)
  }
}

case class Hand(cards: String, tpe: HandType, bid: Int) {
  def sortKey: (Int, Int) = {
    def value(c: Char): Int = c match {
      case 'A' => 14
      case 'K' => 13
      case 'Q' => 12
      case 'J' => 11
      case 'T' => 10
      case _ => c - '0'
    }
    tpe.value -> cards.foldLeft(0) { case (n, c) => n*14 + value(c) }
  }
  def sortKey2: Int = {
    def value(c: Char): Int = c match {
      case 'A' => 14
      case 'K' => 13
      case 'Q' => 12
      case 'J' => 1
      case 'T' => 10
      case _ => c - '0'
    }
    cards.foldLeft(tpe.value) { case (n, c) => n*14 + value(c) }
  }
}

object Hand {
  def parse(line: String): Hand = {
    line match {
      case s"$cards $bid" =>
        assert(cards.length == 5)
        val tpe = HandType.determine(cards)
        Hand(cards, tpe, bid.toInt)
    }
  }
  def parse2(line: String): Hand = {
    line match {
      case s"$cards $bid" =>
        assert(cards.length == 5)
        val tpe = HandType.determine2(cards)
        Hand(cards, tpe, bid.toInt)
    }
  }
}

def part1 = {
  val input = readInput().filterNot(_.isBlank)
  val result = input
    .map(line => Hand.parse(line))
    .sortBy(_.sortKey)
    //.foreach(println)
    .zipWithIndex
    .map((h, index) => h.bid * (index + 1))
    .sum

  println(s"Part 1 - result: $result")
}

def part2 = {
  val input = readInput().filterNot(_.isBlank)
  val result = input
    .map(line => Hand.parse2(line))
    .sortBy(_.sortKey2)
    //.foreach(println)
    .zipWithIndex
    .map((h, index) => h.bid * (index + 1))
    .sum

  println(s"Part 2 - result: $result")
}

@main def main() = {
  part1
  part2
}
