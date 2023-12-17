//> using scala "3.3.1"
//> using jvm zulu:17

import scala.annotation.tailrec
import scala.collection.mutable

def readInput() = {
  val source = scala.io.Source.fromFile("input.txt")
  val input = source.getLines().toSeq
  source.close()
  input
}

case class Platform(width: Int, height: Int, grid: Array[Char]) {
  def apply(x: Int, y: Int): Char = {
    if (y >= height) '.' else grid(y * width + x)
  }

  def update(x: Int, y: Int, c: Char): Unit = {
    if (y < height) grid(y * width + x) = c
  }

  def rotated: Platform = {
    val arr = Array.fill(width * height)(' ')
    val result = Platform(height, width, arr)
    for {
      y <- 0 until height
      x <- 0 until width
      rotatedX = width - y - 1
      rotatedY = x
    } result(rotatedX, rotatedY) = this(x, y)
    result
  }

  override def toString(): String = {
    grid.grouped(width).map(_.mkString).mkString("\n")
  }


  def copy: Platform =
    Platform(width, height, Array.copyOf(grid, width * height))
}

object Platform {
  def fromInput(lines: Seq[String]): Platform = {
      val arr = lines.flatten.toArray
      Platform(lines.head.length, lines.size, arr)
  }
  
  def calculateLoadOnNorth(platform: Platform): Long = 
    platform.grid
      .grouped(platform.width)
      .zipWithIndex
      .map {
        case (row, index) =>
          val loadPerRock = platform.height - index
          row.count(_ == 'O') * loadPerRock
      }.sum
}

def part1 = {
  def tilt(platform: Platform): Platform = {
    def moveRocksOnRow(y: Int): Int = {
      if (y < platform.height) {
        val moved = (0 until platform.width).foldLeft(0)((n, x) =>
            if (platform(x, y) == '.' && platform(x, y + 1) == 'O') {
              platform.update(x, y, 'O')
              platform.update(x, y + 1, '.')
              n + 1
            } else n
        )
        moveRocksOnRow(y + 1) + moved
      } else 0
    }

    def loop: Unit = {
      val moved = moveRocksOnRow(0)
      if (moved > 0) loop else ()
    }

    loop
    platform
  }

  val platform = Platform.fromInput(readInput())
  println(platform)
  tilt(platform)
  println("---")
  println(platform)

  def calculateLoad(platform: Platform): Int = 
    platform.grid
      .grouped(platform.width)
      .zipWithIndex
      .map {
        case (row, index) =>
          val loadPerRock = platform.height - index
          row.count(_ == 'O') * loadPerRock
      }.sum

  val load = Platform.calculateLoadOnNorth(platform)

  println(s"Part 1 - result: $load")
}

def part2 = {
  def tiltW(platform: Platform): Unit = {
    @tailrec
    def moveRocks(y: Int): Unit = {
      var x = 0
      var sx = x + 1
      while (x < platform.width - 1) {
        if (platform(x, y) == '.') {
          while (sx < platform.width && platform(sx, y) == '.')
            sx += 1

          if (sx < platform.width && platform(sx, y) == 'O') {
            // lets drop it!
            platform(sx, y) = '.'
            platform(x, y) = 'O'
          } else {
            x = sx
          }
        }
        sx += 1
        x += 1
      }
      if (y < platform.height - 1)
        moveRocks(y + 1)
      else ()
    }

    moveRocks(0)
  }

  def tiltE(platform: Platform): Unit = {
    @tailrec
    def moveRocks(y: Int): Unit = {
      var x = platform.width - 1
      var sx = x - 1
      while (x > 0) {
        if (platform(x, y) == '.') {
          while (sx >= 0 && platform(sx, y) == '.')
            sx -= 1

          if (sx >= 0 && platform(sx, y) == 'O') {
            // lets drop it!
            platform(sx, y) = '.'
            platform(x, y) = 'O'
          } else {
            x = sx
          }
        }
        sx -= 1
        x -= 1
      }
      if (y < platform.height - 1)
        moveRocks(y + 1)
      else ()
    }

    moveRocks(0)
  }

  def tiltN(platform: Platform): Unit = {
    @tailrec
    def moveRocks(x: Int): Unit = {
      var y = 0
      var sy = y + 1
      while (y < platform.height - 1) {
        if (platform(x, y) == '.') {
          while (sy < platform.height && platform(x, sy) == '.')
            sy += 1

          if (sy < platform.height && platform(x, sy) == 'O') {
            // lets drop it!
            platform(x, sy) = '.'
            platform(x, y) = 'O'
          } else {
            y = sy
          }
        }
        sy += 1
        y += 1
      }
      if (x < platform.width - 1)
        moveRocks(x + 1)
      else ()
    }

    moveRocks(0)
  }

  def tiltS(platform: Platform): Unit = {
    @tailrec
    def moveRocks(x: Int): Unit = {
      var y = platform.height - 1
      var sy = y - 1
      while (y > 0) {
        if (platform(x, y) == '.') {
          while (sy >= 0 && platform(x, sy) == '.')
            sy -= 1

          if (sy >= 0 && platform(x, sy) == 'O') {
            // lets drop it!
            platform(x, sy) = '.'
            platform(x, y) = 'O'
          } else {
            y = sy
          }
        }
        sy -= 1
        y -= 1
      }
      if (x < platform.width - 1)
        moveRocks(x + 1)
      else ()
    }

    moveRocks(0)
  }

  def cycle(platform: Platform): Platform = {
    val p = platform.copy
    tiltN(p)
    tiltW(p)
    tiltS(p)
    tiltE(p)
    p
  }

  type State = Vector[Long]

  def toState(p: Platform): State = {
    p.grid.filterNot(_ == '#')
      .grouped(64)
      .toVector
      .map(arr => arr.zipWithIndex.foldLeft(0L) {
        case (n, ('O', index)) => n | (1 << index)
        case (n, _) => n
      }) 
  }

  def doCycles(platform: Platform, n: Int): Platform = {
    if (n > 0) {
      if ((n & ((4096*32) - 1)) == 0) println(s"cycle $n")
      val res = cycle(platform)
      doCycles(res, n - 1)
    } else platform
  }

  def detectLoop(platform: Platform, from: Int = 0): (Int, Int) = {
    def testLoop(p: Platform, result: Seq[State], begin: Int, end: Int): (Int, Int) = {
      val loopLen = end - begin
      if (result.size < end + loopLen) {
        val np = cycle(p)
        val npSt = toState(np)
        testLoop(np, result :+ npSt, begin, end)
      } else {
        val firstLoop = result.drop(begin).take(loopLen)
        val secondLoop = result.drop(begin + loopLen).take(loopLen)
        if (firstLoop == secondLoop) {
          (begin, end)
        } else loop(p, result)
      }
    }
    def loop(p: Platform, result: Seq[State]): (Int, Int) = {
      val np = cycle(p)
      val npSt = toState(np)

      val currentIndex = result.size
      result.indexWhere(_ == npSt) match {
        case _ if result.size < from => loop(np, result)
        case _ if result.size < 2 => loop(np, result :+ npSt)
        case -1 => loop(np, result :+ npSt)
        case index if currentIndex - index < 2 =>
          loop(np, result :+ npSt)
        case index =>
          testLoop(np, result :+ npSt, index, currentIndex)
      }
    }

    val (a, b) = loop(platform, Vector.empty)
    (a + from, b + from)
  }
  
  val platform = Platform.fromInput(readInput())

  val (loopBegin, loopEnd) = detectLoop(platform)
  val loopLen = loopEnd - loopBegin
  println(s"loop $loopBegin -> $loopEnd : len = $loopLen")

  val cycleN = 1000_000_000
  val toCycleN = cycleN - loopEnd

  val mod = toCycleN % loopLen

  val resultPlatform = doCycles(platform, loopBegin + mod)

  //val resultPlatform = doCycles(memo, platform, 1000_000_000)

  println(resultPlatform)

  //val verify = doCycles(memo, platform, cycleN)
  //println(verify)

  //println("Is same: " + (resultPlatform == verify))

  val load = Platform.calculateLoadOnNorth(resultPlatform)
  println(s"Part 2 - result: $load")
}

@main def main() = {
  //part1
  part2
}

