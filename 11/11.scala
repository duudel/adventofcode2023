//> using scala "3.3.1"
//> using jvm zulu:17

def readInput() = {
  val source = scala.io.Source.fromFile("input.txt")
  val input = source.getLines().toSeq
  source.close()
  input
}

type GalaxyPos = (Int,Int)
  
def part1 = {
  def expand(lines: Seq[String]) = {
    val expandedRows = lines.flatMap(line => if (line.forall(_ == '.')) Seq(line, line) else Seq(line))
    val expandedCols = expandedRows.transpose.flatMap(line => if (line.forall(_ == '.')) Seq(line, line) else Seq(line))
    expandedCols.transpose
  }

  def createMap(lines: Seq[String]) = {
    expand(lines).zipWithIndex.foldLeft(Seq.empty[GalaxyPos]) {
      case (positionsAcc, (line, y)) =>
        line.zipWithIndex.foldLeft(positionsAcc) {
          case (positions, ('#', x)) =>
            positions :+ (x, y)
          case (positions, (c, x)) =>
            positions
        }
    }
  }
  val galaxyMap = createMap(readInput())

  def distance(p1: GalaxyPos, p2: GalaxyPos): Int = {
    val (x1, y1) = p1
    val (x2, y2) = p2
    (x2 - x1).abs + (y2 - y1).abs
  }

  val distances = galaxyMap.toList.tails.flatMap {
    case head :: tail =>
      tail.map(distance(head, _))
    case Nil => Nil
  }.toSeq

  //println(s"Distances (${distances.size}): $distances")
  val result = distances.sum

  println(s"Part 1 - result: $result")
}

def part2 = {
  case class DistanceMap(width: Int, height: Int, xs: Array[Int], ys: Array[Int]) {
    def distanceX(x0: Int, x1: Int): Int = {
      assert(0 <= x0 && x0 < width)
      assert(0 <= x1 && x1 < width)
      val m = x0.min(x1)
      val M = x0.max(x1)
      (m until M).map(xs).sum
    }
    def distanceY(y0: Int, y1: Int): Int = {
      assert(0 <= y0 && y0 < height)
      assert(0 <= y1 && y1 < height)
      //(y0 to y1).map(ys).sum
      val m = y0.min(y1)
      val M = y0.max(y1)
      (m until M).map(ys).sum
    }
  }


  def parse(lines: Seq[String], expansion: Int): (DistanceMap, Seq[GalaxyPos]) = {
    val width = lines.head.length
    val height = lines.size
    val ys = lines.map {
      case line if line.forall(_ == '.') => expansion
      case _ => 1
    }.toArray
    val xs = lines.transpose.map {
      case line if line.forall(_ == '.') => expansion
      case _ => 1
    }.toArray
    val distanceMap = DistanceMap(width, height, xs, ys)

    val galaxies = lines.zipWithIndex.foldLeft(Seq.empty[GalaxyPos]) {
      case (positionsAcc, (line, y)) =>
        line.zipWithIndex.foldLeft(positionsAcc) {
          case (positions, ('#', x)) =>
            positions :+ (x, y)
          case (positions, (c, x)) =>
            positions
        }
    }
    (distanceMap, galaxies)
  }

  val (distanceMap, galaxies) = parse(
    lines = readInput(),
    expansion = 1000_000
  )

  // Solution 1 - first use distance map to correct the positions of galaxies,
  // then calculate the distances as before.
  def solution1 = {
    def correct(dm: DistanceMap, galaxies: Seq[GalaxyPos]): Seq[GalaxyPos] = {
      galaxies.map { case (x, y) =>
        (dm.distanceX(0, x), dm.distanceY(0, y))
      }
    }
    val correctedGalaxies = correct(distanceMap, galaxies)
    //println("Corrected: " + correctedGalaxies)

    def distance(p1: GalaxyPos, p2: GalaxyPos): Long = {
      val (x1, y1) = p1
      val (x2, y2) = p2
      (x2 - x1).abs + (y2 - y1).abs
    }

    val distances = correctedGalaxies.toList.tails.flatMap {
      case head :: tail =>
        tail.map(distance(head, _))
      case Nil => Nil
    }.toSeq
    //println(s"Distances from corrected (${distances0.size}): $distances0")

    val result = distances.sum
    println(s"Part 2.1 - result: $result")
  }

  // Solution 2 - use distance map directly to calculate distances between galaxies.
  def solution2 = {
    def distanceFromMap(p1: GalaxyPos, p2: GalaxyPos): Long = {
      val (x1, y1) = p1
      val (x2, y2) = p2
      val dx = distanceMap.distanceX(x1, x2) 
      val dy = distanceMap.distanceY(y1, y2)
      dx + dy
    }

    val distances = galaxies.toList.tails.flatMap {
      case head :: tail =>
        tail.map(distanceFromMap(head, _))
      case Nil => Nil
    }.toSeq

    //println(s"Distances (${distances.size}): $distances")

    val result = distances.sum
    println(s"Part 2.2 - result: $result")
  }

  solution1
  solution2
}

@main def main() = {
  part1
  part2
}
