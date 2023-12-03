//> using scala "3.3.0"
//> using jvm zulu:17
import scala.languageFeature.existentials

def readInput(): Array[String] = {
  val source = scala.io.Source.fromFile("input.txt")
  val input = source.getLines().toArray
  source.close()
  input
}

case class Schematic(input: Array[String]) {
  def range(y: Int, x0: Int, x1: Int): Option[String] = {
    if (y >= 0 && y < input.size) {
      val row = input(y)
      val res = row.substring(x0.max(0), x1.min(row.length - 1))
      Some(res)
    } else None
  }
}

def part1 = {
  val schematic = Schematic(readInput())

  def isSymbol(c: Char): Boolean = !c.isDigit && c != '.'

  def findSymbol(row: String, y: Int, x0: Int, x1: Int): Boolean = {
    if (x0 > 0 && isSymbol(row(x0 - 1))) true
    else if (x1 < row.length - 1 && isSymbol(row(x1))) true
    else {
      schematic.range(y - 1, x0-1, x1+1)
        .filter(_.exists(isSymbol))
        .orElse(schematic.range(y + 1, x0-1, x1+1))
        .filter(_.exists(isSymbol))
        .nonEmpty
    }
  }

  def printRed(s: String): Unit = {
    print("\u001b[31m")
    print(s)
    print("\u001b[0m")
  }

  def checkNumbers(row: String, y: Int, startX: Int, result: Seq[Int]): Seq[Int] = {
    if (startX == row.length)
      println()
      result
    else {
      val x0 = row.indexWhere(_.isDigit, startX)
      if (x0 == -1) {
        println(row.substring(startX))
        result
      } else {
        print(row.substring(startX, x0))
        var x1 = row.indexWhere(c => !c.isDigit, x0)
        if (x1 == -1) x1 = row.length()

        if (findSymbol(row, y, x0, x1)) {
          val number = row.substring(x0, x1)
          printRed(number)
          checkNumbers(row, y, x1, result :+ number.toInt)
        } else {
          print(row.substring(x0, x1))
          checkNumbers(row, y, x1, result)
        }
      }
    }
  }

  val res = schematic.input.zipWithIndex
    .flatMap((row, y) => checkNumbers(row, y, 0, Seq.empty))
    .sum

  println("Part 1 - result: " + res)
}

case class Number(value: Int, row: Int, x0: Int, x1: Int) {
  def isAdjacent(y: Int, x: Int): Boolean = {
    ((row == y - 1) && (x0 - 1 <= x && x < x1 + 1))
    || ((row == y) && (x0 - 1 == x || x == x1))
    || ((row == y + 1) && (x0 - 1 <= x && x < x1 + 1))
  }
}
case class Gear(row: Int, x: Int, n1: Int, n2: Int)

def part2 = {
  val schematic = Schematic(readInput())

  def isSymbol(c: Char): Boolean = !c.isDigit && c != '.'

  def findSymbol(row: String, y: Int, x0: Int, x1: Int): Boolean = {
    if (x0 > 0 && isSymbol(row(x0 - 1))) true
    else if (x1 < row.length - 1 && isSymbol(row(x1))) true
    else {
      schematic.range(y - 1, x0-1, x1+1)
        .filter(_.exists(isSymbol))
        .orElse(schematic.range(y + 1, x0-1, x1+1))
        .filter(_.exists(isSymbol))
        .nonEmpty
    }
  }

  def printRed(s: String): Unit = {
    print("\u001b[31m")
    print(s)
    print("\u001b[0m")
  }

  def findNumbers(row: String, y: Int, startX: Int, result: Seq[Number]): Seq[Number] = {
    if (startX == row.length)
      println()
      result
    else {
      val x0 = row.indexWhere(_.isDigit, startX)
      if (x0 == -1) {
        println(row.substring(startX))
        result
      } else {
        print(row.substring(startX, x0))
        var x1 = row.indexWhere(c => !c.isDigit, x0)
        if (x1 == -1) x1 = row.length()

        if (findSymbol(row, y, x0, x1)) {
          val value = row.substring(x0, x1)
          printRed(value)
          val number = Number(value.toInt, y, x0, x1)
          findNumbers(row, y, x1, result :+ number)
        } else {
          print(row.substring(x0, x1))
          findNumbers(row, y, x1, result)
        }
      }
    }
  }

  def findGears(row: String, y: Int, x: Int, numbers: Seq[Number], result: Seq[Gear]): Seq[Gear] = {
    if (x >= row.length)
      result
    else if (row(x) == '*') {
      val adjacentNumbers = numbers.filter(_.isAdjacent(y, x))
      if (adjacentNumbers.size == 2) {
        val n1 = adjacentNumbers(0).value
        val n2 = adjacentNumbers(1).value
        findGears(row, y, x + 1, numbers, result :+ Gear(y, x, n1, n2))
      } else {
        findGears(row, y, x + 1, numbers, result)
      }
    } else if (x < row.length - 1) {
      findGears(row, y, x + 1, numbers, result)
    } else {
      result
    }
  }

  val numbers = schematic.input.zipWithIndex
    .flatMap((row, y) => findNumbers(row, y, 0, Seq.empty))

  val gears = schematic.input.zipWithIndex
    .flatMap((row, y) => findGears(row, y, 0, numbers, Seq.empty))

  val res = gears.map(g => g.n1 * g.n2).sum

  println("Part 2 - result: " + res)
}

@main def main = {
  //part1
  part2
}
