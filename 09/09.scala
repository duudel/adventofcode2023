//> using scala "3.3.1"
//> using jvm zulu:17

def readInput(): Seq[String] = {
  val source = scala.io.Source.fromFile("input.txt")
  val input = source.getLines().toSeq
  source.close()
  input
}

def part1 = {
  val histories = readInput().map(line => line.split(' ').map(_.toInt).toIndexedSeq)

  def predict(history: Seq[Int]): Int = {
    val differencies = history
      .zip(history.tail)
      .map((a, b) => b - a)

    //println(s"Diffs: $differencies")

    if (differencies.forall(_ == 0)) {
      history.last
    } else {
      val d = predict(differencies)
      //println(s"Hist $history + $d")
      history.last + d
    }
  }

  val result = histories
    .map(predict)
    //.tapEach(v => println(s"Prediction $v"))
    .sum
  println(s"Part 1 - result $result")
}

def part2 = {
  val histories = readInput().map(line => line.split(' ').map(_.toInt).toIndexedSeq)

  def predict(history: Seq[Int]): Int = {
    val differencies = history
      .zip(history.tail)
      .map((a, b) => b - a)

    //println(s"Diffs: $differencies")

    if (differencies.forall(_ == 0)) {
      history.head
    } else {
      val d = predict(differencies)
      //println(s"Hist $history + $d")
      history.head - d
    }
  }

  val result = histories
    .map(predict)
    //.tapEach(v => println(s"Prediction $v"))
    .sum
  println(s"Part 2 - result $result")
}

@main def main() = {
  part1
  part2
}

