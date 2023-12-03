//> using scala "3.3.0"
//> using jvm zulu:21

import scala.io.Source

def readInput(): Seq[String] = {
  val source = Source.fromFile("input.txt")
  val result = source.getLines.toSeq
  source.close()
  result
}

def part1 = {
  val input = readInput()

  def lineValue(line: String): Int = {
    val first = line.find(_.isDigit).map(_.toInt - '0'.toInt).getOrElse(0)
    val second = line.findLast(_.isDigit).map(_.toInt - '0'.toInt).getOrElse(0)
    val result = first * 10 + second
    //println(line + ": " + first + ", " + second + " = " + result)
    result
  }

  val result = input.foldLeft(0) { case (sum, line) =>
    sum + lineValue(line)
  }

  println(s"Part 1 - result is: $result")
}

def part2 = {
  val input = readInput()

  val search = Map(
    "0" -> 0, "1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6, "7" -> 7, "8" -> 8, "9" -> 9,
    "one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5,
    "six" -> 6, "seven" -> 7,  "eight" -> 8, "nine" -> 9,
  )

  def findFirstNumber(line: String): Int = {
    val (_, value) = search.map { (key, value) =>
      line.indexOf(key) -> value
    }.filterNot(_._1 == -1).minBy(_._1)
    value
  }

  def findLastNumber(line: String): Int = {
    val (_, value) = search.map { (key, value) =>
      line.lastIndexOf(key) -> value
    }.maxBy(_._1)
    value
  }

  def lineValue(line: String): Int = {
    val first = findFirstNumber(line)
    val second = findLastNumber(line)
    val result = first * 10 + second
    println(line + ": " + first + ", " + second + " = " + result)
    result
  }

  val result = input.foldLeft(0) { case (sum, line) =>
    sum + lineValue(line)
  }

  println(s"Part 2 - result is: $result")
}

@main def main() = {
  part1
  part2
}
