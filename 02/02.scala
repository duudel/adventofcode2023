//> using scala "3.3.0"
//> using jvm zulu:17

import scala.io.Source

def readInput(): Seq[String] = {
  val source = Source.fromFile("input.txt")
  val result = source.getLines.toSeq
  source.close()
  result
}

def part1 = {
  val red = 12
  val green = 13
  val blue = 14

  // Returns true, if all draws are possible
  def check(draws: String): Boolean = {
    draws
      .split(';')
      .flatMap(_.split(','))
      .map(_.trim().split(' '))
      .map(numAndColor => {
        println("num and color: " + numAndColor.toSeq)
        assert(numAndColor.size == 2)
        val num = numAndColor(0).toInt
        val color = numAndColor(1)
        val maxNum = color match {
          case "red" => red
          case "green" => green
          case "blue" => blue
          case _ =>
            assert(false)
        }

        num <= maxNum
      })
      .find(_ == false)
      .isEmpty
  }

  // Returns game ID, if game is possible, otherwise 0
  def game(line: String): Int = {
   val game = line.drop("Game ".length)
   val colonIndex = game.indexOf(":")
   val id = game.take(colonIndex).toInt
   println("Game " + id)
   val dice = game.drop(colonIndex + 1) // + 1 to skip colon
   if (check(dice)) {
     id
   } else {
     0
   }
  }

  val input = readInput()

  val result = input.foldLeft(0) { case (sum, line) =>
    sum + game(line)
  }

  println(s"Part 1 - result is: $result")
}

def part2 = {
  def minimumSet(draws: String): (Int, Int, Int) = {
    draws
      .split(';')
      .map { draw =>
        draw.split(',')
          .foldLeft((0,0,0)) { case ((r, g, b), s) =>
            val numAndColor = s.trim().split(' ')
            println("num and color: " + numAndColor.toSeq)
            assert(numAndColor.size == 2)
            val num = numAndColor(0).toInt
            val color = numAndColor(1)
            color match {
              case "red" => (num, g, b)
              case "green" => (r, num, b)
              case "blue" => (r, g, num)
              case _ =>
                assert(false)
            }
          }
      }
      .foldLeft((0,0,0)) { case ((minR, minG, minB), (drawR, drawG, drawB)) =>
        (minR.max(drawR), minG.max(drawG), minB.max(drawB))
      }
  }

  // Returns game ID, if game is possible, otherwise 0
  def game(line: String): Int = {
   val game = line.drop("Game ".length)
   val colonIndex = game.indexOf(":")
   val id = game.take(colonIndex).toInt
   val dice = game.drop(colonIndex + 1) // + 1 to skip colon
   val (red, green, blue) = minimumSet(dice)
   val power = red * green * blue

   println(s"Game $id: $red * $green * $blue = $power")
   power
  }

  val input = readInput()

  val result = input.foldLeft(0) { case (sum, line) =>
    sum + game(line)
  }

  println(s"Part 2 - result is: $result")
}

@main def main() = {
  //part1
  part2
}
