//> using scala "3.3.1"
//> using jvm zulu:17

def readInput() = {
  //val source = scala.io.Source.fromFile("input-example.txt")
  //val source = scala.io.Source.fromFile("input-example2.txt")
  val source = scala.io.Source.fromFile("input.txt")
  val input = source.getLines().toSeq
  source.close()
  input
}

case class Pos(x: Int, y: Int) {
  def right: Pos  = Pos(x + 1, y)
  def down: Pos   = Pos(x, y + 1)
  def left: Pos   = Pos(x - 1, y)
  def up: Pos     = Pos(x, y - 1)
}

// Map, where (0, 0) is at the center
// lowest coordinate, "min", is at (-width/2, -height/2)
// highest coordinate, "max", is at lowest coordinate + (width-1, height-1)
case class Map(width: Int, height: Int, data: Array[Char]) {
  val min: Pos = Pos(-width/2, -height/2)
  val max: Pos = Pos(-width/2+width-1, -height/2+height-1)

  def coordinates: IndexedSeq[Pos] = for {
    y <- min.y to max.y
    x <- min.x to max.x
  } yield Pos(x, y)

  def contains(pos: Pos): Boolean = {
    (min.x <= pos.x && pos.x <= max.x) &&
    (min.y <= pos.y && pos.y <= max.y)
  }

  def apply(pos: Pos): Char = {
    assert(min.x <= pos.x && pos.x <= max.x)
    assert(min.y <= pos.y && pos.y <= max.y)
    val x = pos.x - min.x
    val y = pos.y - min.y 
    data(y * width + x)
  }

  def update(pos: Pos, c: Char): Unit = {
    assert(min.x <= pos.x && pos.x <= max.x)
    assert(min.y <= pos.y && pos.y <= max.y)
    val x = pos.x - min.x
    val y = pos.y - min.y 
    data(y * width + x) = c
  }

  def flood(pos: Pos) = {
    def impl(p: Pos, from: Pos): Unit = {
      if (contains(p) && this(p) == '.') {
        this(p) = '#'
        //println(toString())
        //scala.io.StdIn.readLine()
        (p.x - from.x, p.y - from.y) match {
          case (1, 0) => // going right
            impl(p.right, p)
            impl(p.down, p)
            impl(p.up, p)
          case (-1, 0) => // going left
            impl(p.left, p)
            impl(p.down, p)
            impl(p.up, p)
          case (0, 1) => // going down
            impl(p.down, p)
            impl(p.left, p)
            impl(p.right, p)
          case (0, -1) => // going up
            impl(p.up, p)
            impl(p.left, p)
            impl(p.right, p)
        }
      }
    }

    this(pos) = '#'
    impl(pos.right, pos)
    impl(pos.left, pos)
    impl(pos.up, pos)
    impl(pos.down, pos)
  }

  override def toString(): String = {
    data.grouped(width)
      .map(_.mkString)
      .mkString("\n")
  }
}

object Map {
  def create(w: Int, h: Int): Map = {
    Map(w, h, Array.fill(w * h)('.'))
  }

  def realloc(map: Map): Map = {
    val w = map.width * 2
    val h = map.height * 2
    val result = create(w, h)

    map
      .coordinates
      .foreach(pos => result(pos) = map(pos))

    result
  }
}

def part1 = {
  var map = Map.create(5,5)

  val pos = Pos(0, 0)
  readInput().foldLeft(pos)((pos, line) => line match {
    case s"$dir $num $color" =>
      val n = num.toInt
      var p = pos
      (1 to n).foreach(_ => {
        p = dir match {
          case "R" => p.right
          case "D" => p.down
          case "L" => p.left
          case "U" => p.up
        }
        if (!map.contains(p)) {
          map = Map.realloc(map)
        }
        map(p) = '#'
      })
      p
  })

  map.flood(Pos(1, 1))
  val cubicMeters = map.coordinates.count(p => map(p) == '#')

  println(map)

  println(s"Part 1 - result: $cubicMeters")
}

case class Edge(start: Pos, end: Pos, d: String)

def part2 = {

  type Dir = "R" | "D" | "L" | "U"

  def parseEdges(lines: Seq[String])(parseLine: String => (Dir, Int)): Seq[Edge] = {
    val (_, edges) = lines.foldLeft((Pos(0,0), Vector.empty[Edge])) {
      case ((startPos, edges), line) =>
        val (dir, n) = parseLine(line)
        val endPos = dir match {
          case "R" => Pos(startPos.x + n, startPos.y)
          case "D" => Pos(startPos.x, startPos.y + n)
          case "L" => Pos(startPos.x - n, startPos.y)
          case "U" => Pos(startPos.x, startPos.y - n)
        }
        val edge = Edge(startPos, endPos, dir)
        (endPos, edges :+ edge)
    }
    edges
  }
    
  def parseLineDirNumFromHex(line: String): (Dir, Int) = {
    line match {
      case s"$a $b (#$hex)" =>
        val numS = hex.dropRight(1)
        val num = Integer.parseInt(numS, 16)
        val dir: Dir = hex.last match {
          case '0' => "R"
          case '1' => "D"
          case '2' => "L"
          case '3' => "U"
        }
        (dir, num)
    }
  }

  def parseLineDirNum(line: String): (Dir, Int) = {
    line match {
      case s"R $num $color" => ("R", num.toInt)
      case s"D $num $color" => ("D", num.toInt)
      case s"L $num $color" => ("L", num.toInt)
      case s"U $num $color" => ("U", num.toInt)
    }
  }

  def calculateArea(edges: Seq[Edge]): Long = {
    val areaDoubled = edges.map { case Edge(p1, p2, _) =>
      (p1.x.toLong * p2.y - p2.x.toLong * p1.y).toLong
    }.sum
    
    val edgesArea = edges.map {
      case Edge(p1, p2, "R") => (p2.x - p1.x).toLong
      case Edge(p1, p2, "D") => (p2.y - p1.y).toLong
      case _ => 0L
    }.sum + 1

    areaDoubled.abs / 2 + edgesArea
  }
  
  //val edges = parseEdges(readInput())(parseLineDirNum)
  val edges = parseEdges(readInput())(parseLineDirNumFromHex)

  val area = calculateArea(edges)

  println(s"Part 2 - result: ${area}")
}

@main def main() = {
  //part1
  part2
}

