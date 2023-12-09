//> using scala "3.3.1"
//> using jvm zulu:17

import scala.annotation.tailrec

def readInput(): Seq[String] = {
  val source = scala.io.Source.fromFile("input.txt")
  val input = source.getLines().toSeq
  source.close()
  input
}

enum Instr {
  case L
  case R
}

class Instructions(
  val instr: Seq[Instr]
) {
  def iterator: InstructionsIter = new InstructionsIter(this)
}

class InstructionsIter(is: Instructions) {
  private var index = 0

  def getIndex: Int = index

  def next: Instr = {
    val result = is.instr(index)
    index = (index + 1) % is.instr.size
    result
  }
}

object Instructions {
  def fromString(s: String): Instructions = {
    val instr = s.map {
      case 'L' => Instr.L
      case 'R' => Instr.R
    }
    new Instructions(instr)
  }
}

type Label = String
case class Node(label: Label, left: Label, right: Label)

object Node {
  def fromString(s: String): Node = {
    s match {
      case s"$label = ($left, $right)" => Node(label, left, right)
      case _ => throw MatchError(s"Did not match! '$s'")
    }
  }
}

case class Nodes(map: Map[Label, Node]) {
  def get(label: Label): Node = map(label)

  def all(pred: Label => Boolean): Seq[Label] = map.keys.filter(pred).toVector
}

object Nodes {
  def fromStrings(lines: Seq[String]): Nodes = {
    val nodes = lines.map(Node.fromString)
    val hm = HashMap.from(nodes.map(n => n.label -> n))
    Nodes(hm)
  }
}

def parseInput(inp: Seq[String]): (Instructions, Nodes) = {
  (Instructions.fromString(inp(0)), Nodes.fromStrings(inp.tail.filterNot(_.isBlank)))
}

def part1 = {
  val (instructions, nodes) = parseInput(readInput())

  @tailrec
  def loop(current: Label, nodes: Nodes, it: InstructionsIter, step: Int): Int = {
    if (current == "ZZZ") step
    else {
      val n = nodes.get(current)
      val instr = it.next
      instr match {
        case Instr.L => loop(n.left, nodes, it, step + 1)
        case Instr.R => loop(n.right, nodes, it, step + 1)
      }
    }
  }

  val result = loop("AAA", nodes, instructions.iterator, 0)

  println(s"Part 1 - result $result")
}

def part2 = {
  val (instructions, nodes) = parseInput(readInput())

  type LabelSetPrefix = String
  type Memo = Map[LabelSetPrefix, Int]

  @tailrec
  def loop1(current: Label, nodes: Nodes, it: InstructionsIter, step: Int): Int = {
    if (current.last == 'Z') {
      println(s"step $step it index: ${it.getIndex}")
      step
    } else {
      val instr = it.next
      val n = nodes.get(current)
      val next = instr match {
        case Instr.L => n.left
        case Instr.R => n.right
      }
      loop1(next, nodes, it, step + 1)
    }
  }

  val starts = nodes.all(label => label.last == 'A').toVector
  println(s"Starting nodes (${starts.size}): $starts")
  val ends = nodes.all(label => label.last == 'Z').toVector
  println(s"Ending nodes (${ends.size}): $ends")

  val loopLengths = starts.map(loop1(_, nodes, instructions.iterator, 0))
  // All starting nodes form closed loops. This is clear after solving each starting node individually.
  // Each loop length also seems to be divisible by the instruction count (each loop end node is reached
  // after last instruction before repeating the instructions.) This means there is some number of steps, N,
  // divisible by all of the loop lengths. After N steps, all of the loops have ended at their respective
  // end node. We need to find this N.

  val startsAndLengths = starts.zip(loopLengths).sortBy(_._2).reverse
  println(s"Starts and lengths: ${startsAndLengths.mkString(" ")}")

  @tailrec
  def loopCycles(lens: Seq[Int], steps: Long): Long = {
    val newSteps = steps + lens.head
    if (lens.forall(len => newSteps % len == 0)) newSteps
    else loopCycles(lens, newSteps)
  }

  val result = loopCycles(startsAndLengths.map(_._2), 0)

  println(s"Part 2 - result $result")
}

@main def main() = {
  //part1
  part2
}

