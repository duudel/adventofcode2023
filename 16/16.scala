//> using scala "3.3.1"
//> using jvm zulu:17
import scala.collection.immutable.ArraySeq

def readInput() = {
  val source = scala.io.Source.fromFile("input.txt")
  val input = source.getLines().toSeq
  source.close()
  input
}

case class Pos(x: Int, y: Int) {
  def up: Pos = Pos(x, y - 1)
  def down: Pos = Pos(x, y + 1)
  def left: Pos = Pos(x - 1, y)
  def right: Pos = Pos(x + 1, y)
}

enum Direction {
  case Up
  case Right
  case Down
  case Left
  
  // Up     - 0001
  // Right  - 0010
  // Down   - 0100
  // Left   - 1000
  def toBit: Byte = (1 << this.ordinal).toByte
}

object Direction {
  val all: Seq[Direction] = Seq(Direction.Up, Direction.Right, Direction.Down, Direction.Left)
}

case class Grid(width: Int, height: Int, data: ArraySeq[Char]) {
  def apply(pos: Pos): Option[Char] = {
    if (0 <= pos.x && pos.x < width && 0 <= pos.y && pos.y < height) {
      Some(data(pos.y * width + pos.x))
    } else None
  }

  override def toString(): String = {
    s"$width x $height:\n" + data.grouped(width).map(_.mkString).mkString("\n")
  }
}

object Grid {
  def fromLines(lines: Seq[String]): Grid = {
    val w = lines.head.length
    val h = lines.size
    Grid(w, h, ArraySeq.from(lines.flatten))
  }
}

case class Beam(pos: Pos, dir: Direction) {
  def up: Beam = Beam(pos.up, Direction.Up)
  def down: Beam = Beam(pos.down, Direction.Down)
  def left: Beam = Beam(pos.left, Direction.Left)
  def right: Beam = Beam(pos.right, Direction.Right)

  def splitH: List[Beam] = List(Beam(pos, Direction.Up), Beam(pos, Direction.Down))
  def splitV: List[Beam] = List(Beam(pos, Direction.Right), Beam(pos, Direction.Left))
}

case class EnergyMap(width: Int, height: Int, data: Array[Byte]) {
  def apply(pos: Pos): Seq[Direction] = {
    val bits = data(pos.y * width + pos.x)
    Direction.all.filter(d => (d.toBit & bits) != 0)
  }
  // Returns true, if there was no value for the same direction,
  // meaning the beam is the first one to traverse in this direction.
  def update(pos: Pos, direction: Direction): Boolean = {
    val bits = data(pos.y * width + pos.x)
    val result = (bits & direction.toBit) == 0
    data(pos.y * width + pos.x) = (bits | direction.toBit).toByte
    result
  }

  override def toString(): String = {
    data.grouped(width).map(_.map {
      case 0 => '.'
      case n => '#'
    }.mkString).mkString("\n")
  }
}

object EnergyMap {
  def empty(w: Int, h: Int): EnergyMap =
    EnergyMap(w, h, Array.fill(w * h)(0.toByte))
}

def printMap(grid: Grid, em: EnergyMap, beam: Beam) = {
  val arr = grid.data.toArray
  em.data.zipWithIndex.map((bits, index) => {
    val v = (bits & 5) != 0
    val h = (bits & 10) != 0
    val newC = arr(index) match {
      case '.' if v && h => '+'
      case '.' if v => 'v'
      case '.' if h => 'h'
      case c => c
    }
    arr(index) = newC
  })
  grid(beam.pos) match {
    case Some(_) => 
      val b = beam.dir match {
        case Direction.Up => '^'
        case Direction.Right  => '>'
        case Direction.Down => 'V'
        case Direction.Left  => '<'
      }
      arr(beam.pos.y * grid.width + beam.pos.x) = b
    case None => ()
  }
  arr
    .grouped(grid.width)
    .foreach(row => {
      row.foreach(c => c match {
        case '<' | '>' | 'V' | '^' | '+' =>
          print(s"\u001b[1;95m$c")
        case 'v' =>
          print(s"\u001b[1;95m|")
        case 'h' =>
          print(s"\u001b[1;95m-")
        case '\\' | '/' | '|' | '-' =>
          print(s"\u001b[1;96m$c")
        case _ =>
          print(s"\u001b[21;90m$c")
      })
      println()
  })
  print(s"\u001b[0m")
}

def part1 = {
  val grid = Grid.fromLines(readInput())

  case class Context(grid: Grid, em: EnergyMap)

  def simBeams(ctx: Context, beams: List[Beam]): Unit = {
    def simHorizontal(beam: Beam): List[Beam] = {
      println(s"H: $beam")
      ctx.grid(beam.pos) match {
        case Some(c) =>
          if (ctx.em.update(beam.pos, beam.dir)) {
            // no previous beam traversed in the same direction in this cell
            (c, beam.dir) match {
              case ('/', Direction.Right) => simVertical(beam.up)
              case ('/', Direction.Left) => simVertical(beam.down)
              case ('\\', Direction.Right) => simVertical(beam.down)
              case ('\\', Direction.Left) => simVertical(beam.up)
              case ('|', _) => beam.splitH //List(beam.up, beam.down)
              case (_, Direction.Right) => simHorizontal(beam.right)
              case (_, Direction.Left) => simHorizontal(beam.left)
              case _ => assert(false, s"Invalid configuration for horizontal beam $beam")
            }
          } else {
            println("There was already beam here!")
            Nil
          }
        case None =>
          // beam is out of grid, no new beams spawn
          Nil
      }
    }

    def simVertical(beam: Beam): List[Beam] = {
      ctx.grid(beam.pos) match {
        case Some(c) =>
          if (ctx.em.update(beam.pos, beam.dir)) {
            // no previous beam traversed in the same direction in this cell
            (c, beam.dir) match {
              case ('/', Direction.Up) => simHorizontal(beam.right)
              case ('/', Direction.Down) => simHorizontal(beam.left)
              case ('\\', Direction.Up) => simHorizontal(beam.left)
              case ('\\', Direction.Down) => simHorizontal(beam.right)
              case ('-', _) => beam.splitV //List(beam.left, beam.right)
              case (_, Direction.Up) => simVertical(beam.up)
              case (_, Direction.Down) => simVertical(beam.down)
              case _ => assert(false, s"Invalid configuration for vertical beam $beam")
            }
          } else {
            println("There was already beam here!")
            Nil
          }
        case None =>
          // beam is out of grid, no new beams spawn
          Nil
      }
    }

    def waitKey(): Unit = scala.io.StdIn.readLine()

    def sim(beam: Beam, tail: List[Beam]): Unit = {
      //printMap(ctx.grid, ctx.em, beam)
      //waitKey()
      val newBeams = beam.dir match {
        case Direction.Right | Direction.Left =>
          simHorizontal(beam)
        case Direction.Up | Direction.Down =>
          simVertical(beam)
      }
      (tail ::: newBeams) match {
        case h :: rest => sim(h, rest)
        case Nil => ()
      }
    }

    beams match {
      case h :: rest => sim(h, rest)
      case Nil => ()
    }
  }

  val em = EnergyMap.empty(grid.width, grid.height)
  val ctx = Context(grid, em)
  simBeams(ctx, Beam(Pos(0, 0), Direction.Right) :: Nil)

  println(em)

  val result = em.data.count(_ != 0)

  println(s"Part 1 - result: $result")
}

def part2 = {
  val grid = Grid.fromLines(readInput())

  case class Context(grid: Grid, em: EnergyMap)

  def simBeams(ctx: Context, beams: List[Beam]): Unit = {
    def simHorizontal(beam: Beam): List[Beam] = {
      //println(s"H: $beam")
      ctx.grid(beam.pos) match {
        case Some(c) =>
          if (ctx.em.update(beam.pos, beam.dir)) {
            // no previous beam traversed in the same direction in this cell
            (c, beam.dir) match {
              case ('/', Direction.Right) => simVertical(beam.up)
              case ('/', Direction.Left) => simVertical(beam.down)
              case ('\\', Direction.Right) => simVertical(beam.down)
              case ('\\', Direction.Left) => simVertical(beam.up)
              case ('|', _) => beam.splitH //List(beam.up, beam.down)
              case (_, Direction.Right) => simHorizontal(beam.right)
              case (_, Direction.Left) => simHorizontal(beam.left)
              case _ => assert(false, s"Invalid configuration for horizontal beam $beam")
            }
          } else {
            //println("There was already beam here!")
            Nil
          }
        case None =>
          // beam is out of grid, no new beams spawn
          Nil
      }
    }

    def simVertical(beam: Beam): List[Beam] = {
      ctx.grid(beam.pos) match {
        case Some(c) =>
          if (ctx.em.update(beam.pos, beam.dir)) {
            // no previous beam traversed in the same direction in this cell
            (c, beam.dir) match {
              case ('/', Direction.Up) => simHorizontal(beam.right)
              case ('/', Direction.Down) => simHorizontal(beam.left)
              case ('\\', Direction.Up) => simHorizontal(beam.left)
              case ('\\', Direction.Down) => simHorizontal(beam.right)
              case ('-', _) => beam.splitV //List(beam.left, beam.right)
              case (_, Direction.Up) => simVertical(beam.up)
              case (_, Direction.Down) => simVertical(beam.down)
              case _ => assert(false, s"Invalid configuration for vertical beam $beam")
            }
          } else {
            //println("There was already beam here!")
            Nil
          }
        case None =>
          // beam is out of grid, no new beams spawn
          Nil
      }
    }

    def waitKey(): Unit = scala.io.StdIn.readLine()

    def sim(beam: Beam, tail: List[Beam]): Unit = {
      //printMap(ctx.grid, ctx.em, beam)
      //waitKey()
      val newBeams = beam.dir match {
        case Direction.Right | Direction.Left =>
          simHorizontal(beam)
        case Direction.Up | Direction.Down =>
          simVertical(beam)
      }
      (tail ::: newBeams) match {
        case h :: rest => sim(h, rest)
        case Nil => ()
      }
    }

    beams match {
      case h :: rest => sim(h, rest)
      case Nil => ()
    }
  }

  def simOneBeam(beam: Beam): Int = {
    val em = EnergyMap.empty(grid.width, grid.height)
    val ctx = Context(grid, em)
    simBeams(ctx, beam :: Nil)

    println(em)
    em.data.count(_ != 0)
  }

  def testEachConfiguration(): Seq[(Beam, Int)] = {
    val left = (0 until grid.height).map(y => Beam(Pos(0, y), Direction.Right))
    val top = (0 until grid.width).map(x => Beam(Pos(x, 0), Direction.Down))
    val right = (0 until grid.height).map(y => Beam(Pos(grid.width - 1, y), Direction.Left))
    val bottom = (0 until grid.width).map(x => Beam(Pos(x, grid.height - 1), Direction.Up))
    val beams = left ++ top ++ right ++ bottom
    beams.map(b => b -> simOneBeam(b))
  }

  val results = testEachConfiguration()

  val (beam, result) = results.maxBy(_._2)

  println(s"Part 2 - result: $result - $beam")
}

@main def main() = {
  //part1
  part2
}

