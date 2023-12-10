//> using scala "3.3.1"
//> using jvm zulu:17

import scala.collection.mutable.ArrayBuffer

def readInput(): Seq[String] = {
  val source = scala.io.Source.fromFile("input.txt")
  val input = source.getLines().toSeq
  source.close()
  input
}

enum Direction {
  case North
  case East
  case South
  case West

  def opposite: Direction = this match {
    case North => South
    case East => West
    case South => North
    case West => East
  }
}

enum Tile(val c: Char, val directions: Option[(Direction, Direction)]) {
  case Ground extends Tile('.', None)
  case Start extends Tile('S', None)
  case Vertical extends Tile('|', Some(Direction.North, Direction.South))
  case Horizontal extends Tile('-', Some(Direction.West, Direction.East))
  case NorthToEast extends Tile('L', Some(Direction.North, Direction.East))
  case NorthToWest extends Tile('J', Some(Direction.North, Direction.West))
  case SouthToWest extends Tile('7', Some(Direction.South, Direction.West))
  case SouthToEast extends Tile('F', Some(Direction.South, Direction.East))
  
  def connectsTo(direction: Direction): Boolean = {
    this.directions.exists((d1, d2) => d1 == direction || d2 == direction)
  }
}

case class Pos(x: Int, y: Int) {
  def up: Pos = Pos(x, y - 1)
  def right: Pos = Pos(x + 1, y)
  def down: Pos = Pos(x, y + 1)
  def left: Pos = Pos(x - 1, y)
  def moveTo(dir: Direction): Pos = dir match {
    case Direction.North => up
    case Direction.East => right
    case Direction.South => down
    case Direction.West => left
  }
}

case class PipeGrid(
  width: Int,
  height: Int,
  start: Pos,
  tiles: Array[Tile]
) {
  def apply(p: Pos): Tile = {
    assert(0 <= p.x && p.x < width)
    assert(0 <= p.y && p.y < height)
    tiles(p.y * width + p.x)
  }

  def get(p: Pos): Option[Tile] = {
    if ((0 <= p.x && p.x < width) &&
        (0 <= p.y && p.y < height)) {
      Some(tiles(p.y * width + p.x))
    } else {
      None
    }
  }
}

object PipeGrid {
  def fromLines(lines: Seq[String]): PipeGrid = {
    val width = lines.head.size
    val height = lines.size

    val arrayBuilder = Array.newBuilder[Tile]
    arrayBuilder.sizeHint(width * height)

    val arr = lines.foldLeft(arrayBuilder) { case (b, line) =>
      val tiles = line.map {
        case '.' => Tile.Ground
        case 'S' => Tile.Start
        case '|' => Tile.Vertical
        case '-' => Tile.Horizontal
        case 'L' => Tile.NorthToEast
        case 'J' => Tile.NorthToWest
        case '7' => Tile.SouthToWest
        case 'F' => Tile.SouthToEast
      }
      b.addAll(tiles)
    }.result()

    val startIndex = arr.indexWhere(_ == Tile.Start)
    val startPos = Pos(startIndex % width, startIndex / width)
    val grid = PipeGrid(width, height, startPos, arr)
    def determineStartTile(p: Pos, g: PipeGrid): Tile = {
      val tileN = g.get(p.up).exists(_.connectsTo(Direction.South))
      val tileS = g.get(p.down).exists(_.connectsTo(Direction.North))
      val tileE = g.get(p.right).exists(_.connectsTo(Direction.West))
      val tileW = g.get(p.left).exists(_.connectsTo(Direction.East))

      (tileN, tileS, tileE, tileW) match {
        case (true, true, _, _)         => Tile.Vertical
        case (true, _, true, _)         => Tile.NorthToEast
        case (true, _, _, true)         => Tile.NorthToWest
        case (false, true, true, _)     => Tile.SouthToEast
        case (false, true, _, true)     => Tile.SouthToWest
        case (false, false, true, true) => Tile.Horizontal
        case _ => throw MatchError("Invalid connectivity")
      }
    }
    val startTile = determineStartTile(startPos, grid)
    arr(startIndex) = startTile
    grid
  }

  def print(grid: PipeGrid): Unit = {
    grid.tiles
      .grouped(grid.width)
      .foreach(row => println(row.map(_.c).mkString))
  }
  
}

def part1 = {
  val grid = PipeGrid.fromLines(readInput())
  //PipeGrid.print(grid)

  def takeStep(pos: (Pos, Direction), grid: PipeGrid): (Pos, Direction) = {
    val (p, dir) = pos
    val newPos = p.moveTo(dir)
    val fromDir = dir.opposite
    val newDirection = grid(newPos).directions match {
      case Some((d1, d2)) if d1 == fromDir => d2
      case Some((d1, d2)) if d2 == fromDir => d1
      case _ =>
        val tile = grid(p)
        throw new MatchError(s"Invalid tile! $pos from $fromDir, $tile")
    }
    (newPos, newDirection)
  }

  def traverse(pos1: (Pos, Direction), pos2: (Pos, Direction), grid: PipeGrid, step: Int): Int = {
    val newPos1 = takeStep(pos1, grid)
    val newPos2 = takeStep(pos2, grid)
    if (newPos1._1 == newPos2._1) {
      step + 1
    } else {
      traverse(newPos1, newPos2, grid, step + 1)
    }
  }

  val startTile = grid(grid.start)
  println(s"Start tile $startTile: ${startTile.directions}")
  val start1 = (grid.start, startTile.directions.get._1)
  val start2 = (grid.start, startTile.directions.get._2)
  val steps = traverse(start1, start2, grid, 0)

  println(s"Part 1 - result: $steps")
}

case class PipeMap(width: Int, height: Int, arr: Array[Int]) {
  def apply(p: Pos): Int = {
    assert(0 <= p.x && p.x < width)
    assert(0 <= p.y && p.y < height)
    arr(p.y * width + p.x)
  }
  def set(p: Pos, i: Int): Unit = {
    arr(p.y * width + p.x) = i
  }
}

object PipeMap {
  def empty(width: Int, height: Int): PipeMap = {
    val arr = Array.fill(width * height)(0)
    PipeMap(width, height, arr)
  }

  def print(m: PipeMap): Unit = {
    m.arr
      .map {
        case 0 => ' '
        case n => n.toString
      }
      .grouped(m.width)
      .foreach(row => println(row.mkString))
  }
}

def part2 = {
  def tileToMap(t: Tile): Int = {
    // Only tiles with north connection, are counted as walls = 2
    t match {
      case Tile.Ground => 0
      case Tile.Start => 0
      case Tile.Horizontal => 1
      case Tile.SouthToWest => 1
      case Tile.SouthToEast => 1
      case Tile.Vertical => 2
      case Tile.NorthToEast => 2
      case Tile.NorthToWest => 2
    }
  }

  def takeStep(pos: (Pos, Direction), grid: PipeGrid, map: PipeMap): (Pos, Direction) = {
    val (p, dir) = pos
    val n = tileToMap(grid(p))
    map.set(p, n)
    val newPos = p.moveTo(dir)
    val fromDir = dir.opposite
    val newDirection = grid(newPos).directions match {
      case Some((d1, d2)) if d1 == fromDir => d2
      case Some((d1, d2)) if d2 == fromDir => d1
      case _ =>
        val tile = grid(p)
        throw new MatchError(s"Invalid tile! $pos from $fromDir, $tile")
    }
    (newPos, newDirection)
  }

  def traverse(pos1: (Pos, Direction), pos2: (Pos, Direction), grid: PipeGrid, map: PipeMap): Unit = {
    val newPos1 = takeStep(pos1, grid, map)
    val newPos2 = takeStep(pos2, grid, map)
    if (newPos1._1 == newPos2._1) {
      val n = tileToMap(grid(newPos1._1))
      map.set(newPos1._1, n)
    } else {
      traverse(newPos1, newPos2, grid, map)
    }
  }

  val grid = PipeGrid.fromLines(readInput())
  //PipeGrid.print(grid)

  val map = PipeMap.empty(grid.width, grid.height)
  
  val startTile = grid(grid.start)
  val start1 = (grid.start, startTile.directions.get._1)
  val start2 = (grid.start, startTile.directions.get._2)
  traverse(start1, start2, grid, map)

  //PipeMap.print(map)

  def countArea(map: PipeMap): Int = {
    def stepX(p: Pos, prev: Int, inside: Boolean, result: Int): Int = {
      if (p.x >= map.width) {
        result
      } else {
        val current = map(p)
        //val newInside = if (prev < 2 && current == 2) !inside else inside
        val newInside = if (current == 2) !inside else inside
        if (current > 1) {
          //if (newInside) print('I') else print('P')
          if (newInside) print('<') else print('>')
          stepX(p.right, current, newInside, result)
        } else {
          if (newInside && current == 0) {
            print('*')
            stepX(p.right, current, newInside, result + 1)
          } else {
            if (current > 0) print('.') else print('_')
            stepX(p.right, current, newInside, result)
          }
        }
      }
    }

    def stepY(y: Int, result: Int): Int = {
      if (y >= map.height) {
        result
      } else {
        val res = stepX(Pos(0, y), 0, false, 0)
        println()
        stepY(y + 1, result + res)
      }
    }

    stepY(0, 0)
  }

  val result = countArea(map)
  println(s"Part 2 - result: $result")
}

@main def main() = {
  part1
  part2
}

