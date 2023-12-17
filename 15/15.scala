//> using scala "3.3.1"
//> using jvm zulu:17

def readInput() = {
  val source = scala.io.Source.fromFile("input.txt")
  val input = source.getLines().toSeq
  source.close()
  input
}

def hash(s: String): Int = {
  s.foldLeft(0) { case (h, c) =>
    (h + c.toInt) * 17 % 256
  }
}

def part1 = {
  val steps = readInput().flatMap(_.split(','))
  val result = steps.map(hash).sum
  println(s"Part 1 - result: $result")
}

case class Lens(label: String, focalLen: Int)

case class Box(lenses: Vector[Lens]) {
  def add(label: String, focalLen: Int): Box = {
    val newLenses = lenses.indexWhere(_.label == label) match {
      case -1 => lenses :+ Lens(label, focalLen)
      case index => lenses.updated(index, Lens(label, focalLen))
    }
    Box(newLenses)
  }

  def remove(label: String): Box = {
    Box(lenses.filterNot(_.label == label))
  }

  def value: Int =
    lenses
      .zipWithIndex
      .map((lens, index) => (index + 1) * lens.focalLen)
      .sum
}

case class Boxes(boxes: Array[Box]) {
  def add(label: String, focalLen: Int): Boxes = {
    val h = hash(label)
    val box = boxes(h)
    boxes(h) = box.add(label, focalLen)
    this
  }

  def remove(label: String): Boxes = {
    val h = hash(label)
    val box = boxes(h)
    boxes(h) = box.remove(label)
    this
  }

  def print: Unit = {
    boxes.zipWithIndex.foreach((box, index) => if (box.lenses.nonEmpty) {
      println(s"Box $index:" + box.lenses.mkString(", "))
    })
  }
}

def part2 = {
  val steps = readInput().flatMap(_.split(','))
  val boxesInit = Boxes(Array.fill(256)(Box(Vector.empty)))
  val boxes = steps.foldLeft(boxesInit) {
    case (bs, step) =>
      step match {
        case s"$label=$fl" =>
          bs.add(label, fl.toInt)
        case s"$label-" =>
          bs.remove(label)
    }
  }

  //boxes.print

  val result = boxes.boxes
    .zipWithIndex
    .map((b, index) => (index + 1) * b.value)
    .sum
  println(s"Part 2 - result: $result")
}

@main def main() = {
  part1
  part2
}

