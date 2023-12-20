//> using scala "3.3.1"
//> using jvm zulu:17

import scala.collection.immutable.ArraySeq
import scala.annotation.tailrec
import scala.collection.immutable.Stream.cons

def readInput() = {
  val source = scala.io.Source.fromFile("input.txt")
  //val source = scala.io.Source.fromFile("input-example2.txt")
  val input = source.getLines().toSeq
  source.close()
  input
}

def timed[A](block: => A) = {
  val start = System.nanoTime()
  val result = block
  val end = System.nanoTime()
  val elapsed = end - start
  println(s"Took ${elapsed/1000_000} ms")
  result
}

case class Pos(x: Int, y: Int) {
  //def move(delta: (Int, Int)): Pos = Pos(x + delta._1, y + delta._2)
  def move(dir: Dir): Pos = {
    dir match {
      case Dir.Left   => Pos(x - 1, y)
      case Dir.Right  => Pos(x + 1, y)
      case Dir.Up     => Pos(x, y - 1)
      case Dir.Down   => Pos(x, y + 1)
    }
  }
}

enum Dir {
  case Left
  case Right
  case Up
  case Down

  def turnLeft: Dir = this match {
    case Left   => Down
    case Right  => Up
    case Up     => Left
    case Down   => Right
  }
  def turnRight: Dir = this match {
    case Left   => Up
    case Right  => Down
    case Up     => Right
    case Down   => Left
  }
}

case class City(width: Int, height: Int, blocks: ArraySeq[Byte]) {
  def get(p: Pos): Option[Byte] = {
    if (0 <= p.x && p.x < width && 0 <= p.y && p.y < height) {
      Some(blocks(p.y * width + p.x))
    } else None
  }

  override def toString(): String = {
    blocks.grouped(width).map(_.mkString).mkString("\n")
  }
}

object City {
  def fromLines(lines: Seq[String]): City = {
    val w = lines.head.length
    val h = lines.size
    City(w, h, ArraySeq.from(lines.flatten.map(c => (c - '0').toByte)))
  }
}

case class LossMap(width: Int, height: Int, maxConsecutive: Int, data: Array[Int]) {
  def update(p: Pos, dir: Dir, consecutive: Int, loss: Int): Boolean = {
    val config = dir.ordinal * maxConsecutive + (consecutive - 1)
    val index = (p.y * width + p.x) * LossMap.configsPerBlock(maxConsecutive) + config

    val prev = data(index)
    if (loss < prev) {
      data(index) = loss
      true
    } else {
      false
    }
  }

  override def toString(): String = {
    data
      .grouped(width)
      .map(row => row.map(i => if (i == Int.MaxValue) " # |" else "%3d|".format(i)).mkString)
      .mkString("\n")
  }
}

object LossMap {
  def configsPerBlock(maxConsecutive: Int): Int = {
    val directions = 4
    directions * maxConsecutive
  }

  def empty(w: Int, h: Int): LossMap = {
    LossMap.empty(w, h, 3)
  }
  def empty(w: Int, h: Int, maxConsecutive: Int): LossMap = {
    LossMap(w, h, maxConsecutive, Array.fill(w * h * configsPerBlock(maxConsecutive))(Int.MaxValue))
  }
}

case class Ref[A](var value: A)

def printMap(city: City, pos: Pos, path: Seq[Pos]) = {
  val arr = city.blocks.map(b => ('0' + b).toChar.toString).toArray
  (Pos(0, 0) +: path).zip(path)
    .foreach((p0, p1) => {
      val c = (p1.x-p0.x) -> (p1.y-p0.y) match {
        case (-1, 0) => "<"
        case (1,  0) => ">"
        case (0, -1) => "^"
        case (0,  1) => "v"
      }
      arr(p1.y * city.width + p1.x) = s"\u001b[93m$c\u001b[90m"
  })

  arr(pos.y * city.width + pos.x) = "\u001b[32;1m*\u001b[90m"

  print("\u001b[90m")
  arr
    .grouped(city.width)
    .foreach(row => println(row.mkString))
  print("\u001b[0m")

  //scala.io.StdIn.readLine()
}

def part1 = {
  def minimizeHeatLoss(city: City): Int = {
    val goal = Pos(city.width-1, city.height-1)

    var found = 0

    @tailrec
    def traverseX(
      prevPos: Pos, dir: Dir, consecutive: Int,
      nextSteps: List[(Pos, Dir, Int, Int, Seq[Pos])],
      lossMap: LossMap,
      result: Int, best: Ref[Int], path: Seq[Pos] = Vector.empty
    ): Option[Int] = {
      val pos = prevPos.move(dir)
      city.get(pos) match {
        case Some(loss) if lossMap.update(pos, dir, consecutive, result + loss) =>
          val totalLoss = result + loss
          if (totalLoss >= best.value) {
              nextSteps match {
                case Nil =>
                  None
                case (p, d, c, totalLoss, path) :: rest =>
                  traverseX(p, d, c, rest, lossMap, totalLoss, best, path)
              }
          } else {
            //println(s"Pos $pos - $delta - c=$consecutive - loss=$totalLoss")
            if (pos == goal) {
              //println(s"Found goal! - $totalLoss")
              //printMap(city, pos, path)
              if (totalLoss < best.value)
                best.value = totalLoss
              found += 1
              //Some(totalLoss)
              nextSteps match {
                case Nil =>
                  None
                case (p, d, c, tot, path) :: rest =>
                  traverseX(p, d, c, rest, lossMap, tot, best, path)
              }
            } else {
              val left = (pos, dir.turnLeft, 1, totalLoss, path :+ pos)
              val right = (pos, dir.turnRight, 1, totalLoss, path :+ pos)
              val next = if (consecutive < 3) {
                left :: (pos, dir, consecutive + 1, totalLoss, path :+ pos) :: right :: Nil
                //(pos, dir, consecutive + 1, totalLoss, path :+ pos) :: left :: right :: Nil
              } else {
                left :: right :: Nil
              }
              nextSteps match {
                case Nil =>
                  None
                case (p, d, c, tot, path) :: rest =>
                  traverseX(p, d, c, next ::: rest, lossMap, tot, best, path)
              }
            }
          }
        case _ =>
          nextSteps match {
            case Nil =>
              None
            case (p, d, c, tot, path) :: rest =>
              traverseX(p, d, c, rest, lossMap, tot, best, path)
          }
      }
    }


    def traverse(
      prevPos: Pos, dir: Dir, consecutiveSteps: Int,
      lossMap: LossMap, result: Int, best: Ref[Int], path: Seq[Pos]
    ): Option[Int] = {

      val pos = prevPos.move(dir)

      city.get(pos) match {
        case Some(loss) if lossMap.update(pos, dir, consecutiveSteps, result + loss) =>
          val totalLoss = result + loss
          if (totalLoss >= best.value) {
            None
          } else {
            if (pos == goal) {
              println(s"Found goal! - $totalLoss")
              //printMap(city, pos, path)
              if (totalLoss < best.value)
                best.value = totalLoss
              found += 1
              Some(totalLoss)
            } else {
              //println(s"pos $pos - $delta - loss = ${result+loss}, $consecutiveSteps")
              //printMap(city, pos, path)

              val newPath = path :+ pos

              def distanceToGoal(d: Dir, c: Int): Int = {
                val p = pos.move(d)
                city.get(p).map(_.toInt).map(loss =>
                  (goal.x - p.x).abs + (goal.y - p.y).abs
                ).getOrElse(Int.MaxValue)
                //(goal.x - p.x).abs + (goal.y - p.y).abs 
              }

              if (false) {
                val directions = Vector(
                  dir.turnLeft -> 1,
                  dir -> (consecutiveSteps + 1),
                  dir.turnRight -> 1,
                ).filter(_._2 <= 3)
                 //.sortBy(distanceToGoal)

                //val dirs = directions.head +: scala.util.Random.shuffle(directions.tail)
                val min = directions.minBy(distanceToGoal)
                val dirs = min +: directions.filterNot(_ == min)
                dirs
                  .flatMap((d, c) => traverse(pos, d, c, lossMap, totalLoss, best, newPath))
                  .minOption
              } else if (false) {
                val directions = Vector(
                  dir.turnLeft -> 1,
                  dir -> (consecutiveSteps + 1),
                  dir.turnRight -> 1,
                )
                directions
                  .filter(_._2 <= 3)
                  .sortBy(distanceToGoal)
                  .flatMap((d, c) => traverse(pos, d, c, lossMap, totalLoss, best, newPath))
                  .minOption
              } else if (false) {
                val directions = scala.util.Random.shuffle(Vector(
                  dir.turnRight -> 1,
                  dir.turnLeft -> 1,
                  dir -> (consecutiveSteps + 1)
                ))
                directions
                  .filter(_._2 <= 3)
                  .flatMap((d, c) => traverse(pos, d, c, lossMap, totalLoss, best, newPath))
                  .minOption
              } else {
                if (consecutiveSteps < 3) {
                  Seq(
                    traverse(pos, dir.turnRight, 1, lossMap, totalLoss, best, path :+ pos),
                    traverse(pos, dir, consecutiveSteps + 1, lossMap, totalLoss, best, path :+ pos),
                    traverse(pos, dir.turnLeft, 1, lossMap, totalLoss, best, path :+ pos),
                  ).flatten.minOption
                } else {
                  Seq(
                    traverse(pos, dir.turnRight, 1, lossMap, totalLoss, best, path :+ pos),
                    traverse(pos, dir.turnLeft, 1, lossMap, totalLoss, best, path :+ pos),
                  ).flatten.minOption
                }
              }
            }
          }
        case _ =>
          None
      }
    }

    if (false) {
      val lMap = LossMap.empty(city.width, city.height)
      val steps = List((Pos(0,0), Dir.Down, 1, 0, Vector.empty))
      //val best: Ref[Int] = Ref(Int.MaxValue)
      val best: Ref[Int] = Ref(city.width * city.height)
      traverseX(Pos(0,0), Dir.Right, 1, steps, lMap, 0, best)

      println(s"traverseX: ${best.value} - $found")
      best.value
    } else {
      val lossMap = LossMap.empty(city.width, city.height)
      val res = for {
        a <- traverse(Pos(0,0), Dir.Right, 1, lossMap, 0, Ref(Int.MaxValue), Vector.empty)
        b <- traverse(Pos(0,0), Dir.Down, 1, lossMap, 0, Ref(a), Vector.empty).orElse(Some(Int.MaxValue))
      } yield a.min(b)
      //println(s"Loss:\n$lossMap")
      println(s"Goal found $found times")
      res.getOrElse(-1)
    }
  }

  val city = City.fromLines(readInput())
  val result = timed(minimizeHeatLoss(city))

  println(s"Part 1 - result: $result")
}

def part2 = {
  def minimizeHeatLoss(city: City): Int = {
    val goal = Pos(city.width-1, city.height-1)

    case class Result(loss: Int, path: Seq[Pos])

    var found = 0

    //def traverseUntil4(pos: Pos, dir: Dir, consecutive: Int, lossMap: LossMap, result: Int, minMaxLoss: Int, best: Ref[Result], path: Seq[Pos]): Unit = {
    //  city.get(pos).map { loss =>
    //    val totalLoss = result + loss
    //    if (totalLoss >= best.value.loss) ()
    //    else if (totalLoss >= minMaxLoss) ()
    //    else if (lossMap.update(pos, dir, consecutive, totalLoss)) {
    //      val maxLossToGoal = distance(pos, goal) * 9
    //      val minMaxL = minMaxLoss.min(totalLoss + maxLossToGoal)
    //      if (consecutive == 3) {
    //        traverse(pos.move(dir), dir, consecutive + 1, lossMap, totalLoss, minMaxL, best, path :+ pos)
    //      } else {
    //        traverseUntil4(pos.move(dir), dir, consecutive + 1, lossMap, totalLoss, minMaxL, best, path :+ pos)
    //      }
    //    }
    //  }
    //}

    def distance(a: Pos, b: Pos): Int = {
      (b.x - a.x).abs + (b.y - a.y).abs
    }

    def traverse(pos: Pos, dir: Dir, consecutive: Int, lossMap: LossMap, result: Int, minMaxLoss: Int, best: Ref[Result], path: Seq[Pos]): Unit = {
      city.get(pos) match {
        case Some(loss) =>
          val totalLoss = result + loss
          if (totalLoss >= best.value.loss) ()
          else if (totalLoss >= minMaxLoss) ()
          else if (lossMap.update(pos, dir, consecutive, totalLoss)) {
            val maxLossToGoal = (distance(pos, goal)) * 9
            val minMaxL = minMaxLoss.min(totalLoss + maxLossToGoal)
            if (consecutive < 4) {
              // must continue forward
              //traverse(pos.move(dir), dir, consecutive + 1, lossMap, totalLoss, best)
              traverse(pos.move(dir), dir, consecutive + 1, lossMap, totalLoss, minMaxL, best, path :+ pos)
            } else if (consecutive < 10) {
              if (pos == goal) {
                println(s"Found goal! loss=$totalLoss")
                found += 1
                if (found % 100 == 0) printMap(city, pos, path)
                if (totalLoss < best.value.loss)
                  best.value = best.value.copy(totalLoss, path)
              } else {
                //val options = for {
                //  (dir, cons) <- Seq(dir.turnLeft -> 1, dir.turnRight -> 1, dir -> (consecutive + 1))
                //  p = pos.move(dir)
                //} yield (p, dir, cons, distance(p, goal))

                //options
                //  .sortBy(_._4)
                //  .foreach((p, d, cons, _) =>
                //    traverse(p, d, cons, lossMap, totalLoss, minMaxL, best, path :+ pos)
                //  )

                val left = dir.turnLeft
                traverse(pos.move(left), left, 1, lossMap, totalLoss, minMaxL, best, path :+ pos)
                
                traverse(pos.move(dir), dir, consecutive + 1, lossMap, totalLoss, minMaxL, best, path :+ pos)

                val right = dir.turnRight
                traverse(pos.move(right), right, 1, lossMap, totalLoss, minMaxL, best, path :+ pos)
              }
            } else {
              // must turn
              val left = dir.turnLeft
              val leftPos = pos.move(left)
              val right = dir.turnRight
              val rightPos = pos.move(right)
              if (distance(leftPos, goal) < distance(rightPos, goal)) {
                traverse(leftPos, left, 1, lossMap, totalLoss, minMaxL, best, path :+ pos)
                traverse(rightPos, right, 1, lossMap, totalLoss, minMaxL, best, path :+ pos)
              } else {
                traverse(rightPos, right, 1, lossMap, totalLoss, minMaxL, best, path :+ pos)
                traverse(leftPos, left, 1, lossMap, totalLoss, minMaxL, best, path :+ pos)
              }
            }
          }
        case None => ()
      }
    }

    val best = Ref(Result(Int.MaxValue, Seq.empty))
    val lm = LossMap.empty(city.width, city.height, 10)
    traverse(Pos(1, 0), Dir.Right, 1, lm, 0, Int.MaxValue, best, Vector.empty)
    traverse(Pos(0, 1), Dir.Down, 1, lm, 0, Int.MaxValue, best, Vector.empty)

    printMap(city, goal, best.value.path)

    best.value.loss
  }

  val city = City.fromLines(readInput())
  val result = timed(minimizeHeatLoss(city))

  println(s"Part 2 - result: $result")
}

@main def main() = {
  //part1
  part2
}
