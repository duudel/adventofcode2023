//> using scala "3.3.0"
//> using jvm zulu:17

def readInput(): Seq[String] = {
  val source = scala.io.Source.fromFile("input.txt")
  val input = source.getLines().toIndexedSeq
  source.close()
  input
}

def parseNumbers(str: String): Set[Int] = {
  str
    .trim()
    .split(' ')
    .toSet
    .filterNot(_.isBlank)
    .map(_.toInt)
}

def part1 = {
  val result = readInput().map { line =>
    line match {
      case s"Card $id: $winning | $my" =>
        val winningNumbers = parseNumbers(winning)
        val myNumbers = parseNumbers(my)
        val myWinningNumbers = winningNumbers.intersect(myNumbers)
        val n = myWinningNumbers.size
        val points = if (n == 0) 0 else (1 << (n - 1))
        println(s"$id: ${winningNumbers.mkString(",")} | ${myNumbers.mkString(",")} => ${myWinningNumbers.mkString(",")} => $points")

        points
    }
  }.sum

  println(s"Part 1 - result: $result")
}

def part2 = {
  def parseCard(line: String) = line match {
    case s"Card $id: $winning | $my" =>
      val winningNumbers = parseNumbers(winning)
      val myNumbers = parseNumbers(my)
      val myWinningNumbers = winningNumbers.intersect(myNumbers)
      myWinningNumbers.size
  }
  val cards = readInput().map(line => parseCard(line)).zipWithIndex

  val cardCounts = Array.fill(cards.size)(1)

  cards.foreach { (n, index) =>
    val multiplier = cardCounts(index)
    (1 to n).foreach(i => cardCounts(index + i) += multiplier)
  }

  val result = cardCounts.sum
  println(s"Part 2 - result: $result")
}

@main def main(): Unit = {
  //part1
  part2
}
