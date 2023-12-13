//> using scala "3.3.1"
//> using jvm zulu:17
import scala.collection.mutable

def readInput(): Seq[String] = {
  val source = scala.io.Source.fromFile("input.txt")
  val input = source.getLines().toSeq
  source.close()
  input
}

// Modeling the matrix rows as base 10 numbers, 1 is rock, 0 is ash
// Using binary would have been nicer, but that ship sailed already.
case class Matrix(w: Int, h: Int, values: Seq[Long]) {
  def transpose: Matrix = Matrix(h, w, values.transpose((value: Long) => {
    var v = value
    val result = scala.collection.mutable.IndexedSeq.fill(w)(0L)
    var index = 0
    while (index < w) {
      val n = v % 10
      result.update(index, n)
      v = v / 10
      index += 1
    }
    result
  }).map(xs => xs.foldLeft(0L)((num, x) => num * 10 + x)).reverse)

  override def toString(): String = {
    values.map(v => {
      val s = v.toString()
      "0" * (w - s.length) + s
    }).mkString("\n")
  }
}

def parse(lines: Seq[String]): Seq[Matrix] = {
  def parseMatrix(lines: Seq[String], result: Seq[Matrix]): Seq[Matrix] = {
    val skipped = lines.dropWhile(_.isBlank)
    if (skipped.isEmpty) result else {
      val (fst, rest) = skipped.span(s => !s.isBlank)

      val w = fst.head.length
      val h = fst.size
      val values: Seq[Long] = fst.map(_.foldLeft(0L) {
        case (num, '.') => num * 10
        case (num, '#') => num * 10 + 1
      })
      val mat = Matrix(w, h, values)
      parseMatrix(rest, result :+ mat)
    }
  }

  parseMatrix(lines, Seq.empty)
}

def part1 = {
  def isReflection(v1: Seq[Long], v2: Seq[Long]): Boolean = {
    def impl(n: Int): Boolean = {
      if (v1(v1.size - n - 1) == v2(n)) {
        if (n + 1 < v1.size && n + 1 < v2.size) {
          impl(n + 1)
        } else true
      } else false
    }

    impl(0)
  }

  def findReflection1(m: Matrix, n: Int): Int = {
    val (v1, v2) = m.values.splitAt(n)
    if (isReflection(v1, v2)) {
      println(s"Reflection! $n")
      n
    } else if (n < m.h - 1) {
      findReflection1(m, n + 1)
    } else {
      0 // No reflection
    }
  }

  def findReflection(m: Matrix): Int = {
    val v = findReflection1(m, 1)
    val h = findReflection1(m.transpose, 1)
    v * 100 + h
  }

  val matrices = parse(readInput())
  matrices.foreach(m => println(m.toString() + "\n----\n"))
  matrices.foreach(m => println(m.transpose.toString() + "\n----\n"))

  val results = matrices.map(findReflection)
  //println(results)

  val result = results.sum

  println(s"Part 1 - result: $result")
}

def part2 = {
  val matrices = parse(readInput())

  def isSmudgedReflection(m1: Seq[Long], m2: Seq[Long]): Boolean = {
    def clean(n: Int): Boolean = {
      if (n == m1.size || n == m2.size) {
        true
      } else if (m1(m1.size - n - 1) == m2(n)) {
        clean(n + 1)
      } else false
    }

    def smudgeAllowed(n: Int): Boolean = {
      if (n == m1.size || n == m2.size) {
        false // do not accept clean reflection
      } else {
        val v1 = m1(m1.size - n - 1)
        val v2 = m2(n)
        if (v1 == v2) {
          smudgeAllowed(n + 1)
        } else {
          var v = (v1 - v2).abs
          var sumOfDigits = 0
          while (sumOfDigits < 2 && v > 0) {
            sumOfDigits += (v % 10).toInt
            v = v / 10
          }

          if (sumOfDigits == 1) {
          //val log = math.log10((v1 - v2).abs)
          //if (log.isWhole) {
            clean(n + 1) // only one smudge, rest needs to be clean reflection
          } else false
        }
      }
    }

    smudgeAllowed(0)
  }

  def findReflectionWithSmudge1(m: Matrix, n: Int): Int = {
    val (v1, v2) = m.values.splitAt(n)
    if (isSmudgedReflection(v1, v2)) {
      println(s"Reflection! $n")
      n
    } else if (n < m.h - 1) {
      findReflectionWithSmudge1(m, n + 1)
    } else {
      0 // No reflection
    }
  }

  def findReflectionWithSmudge(m: Matrix): Int = {
    val v = findReflectionWithSmudge1(m, 1)
    val h = findReflectionWithSmudge1(m.transpose, 1)
    v * 100 + h
  }

  val results = matrices.map(findReflectionWithSmudge)
  println(results)
  val result = results.sum
  println(s"Part 2 - result: $result")
}

@main def main() = {
  //part1
  part2
}
