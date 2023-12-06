//> using scala "3.3.1"
//> using jvm zulu:17

def readInput(): Seq[String] = {
  val source = scala.io.Source.fromFile("input.txt")
  val input = source.getLines().toSeq
  source.close()
  input
}

def part1 = {
  def solve(time: Int, targetDist: Int): Int = {
    def travel(speed: Int, t: Int) = speed * t
    val travels = for {
      // from 1 to (time - 1)
      t <- 1 until time
      speed = time - t
    } yield travel(speed, t)

    travels.count(_ > targetDist)
  }

  val input = readInput()
  val times = input(0) match {
    case s"Time: $times" =>
      times.trim.split(' ').toIndexedSeq.filterNot(_.isBlank).map(_.toInt)
  }
  val distances = input(1) match {
    case s"Distance: $dists" =>
      dists.trim.split(' ').toIndexedSeq.filterNot(_.isBlank).map(_.toInt)
  }
  val results = times.zip(distances).map((t, d) => solve(t, d))

  val result = results.reduce(_ * _)
  println(s"Part 1 - result: $result")
}

def part2 = {
  val input = readInput()
  val time = input(0) match {
    case s"Time: $times" =>
      times.filterNot(_.isSpaceChar).mkString.toLong
  }
  val distance = input(1) match {
    case s"Distance: $dists" =>
      dists.filterNot(_.isSpaceChar).mkString.toLong
  }
  println(s"Time $time, target distance: $distance")
  def travelDistance(speed: Long, t: Long) = speed * t
  
  def solve(time: Long, targetDist: Long): Long = {
    def travel(n: Long): Long = travelDistance(n, time - n)

    def search(n: Long): Long = {
      val dist = travel(n)
      if (dist > targetDist) {
        search(n - 1)
      } else {
        n
      }
    }

    def loop(n: Long): Long = {
      if (travel(n) < targetDist) {
        loop(n * 5)
      } else {
        search(n)
      }
    }

    val n = loop(10)
    val firstWinningN = if (travel(n) < targetDist) {
      // We did not hit exactly the target distance,
      // so need to go one up to get a winning configuration.
      n + 1
    } else n
    
    val dist = travel(firstWinningN)
    println(s"Distance = $dist = target distance + ${dist - targetDist}")


    // .........|------|.........
    // no cigar  winwin  no cigar
    //          \
    //|---------|first winning
    //
    // the problem is symmetric, so once we know from which number
    // onwards we are winning, it will end at the same distance from
    // the end.
    //
    // no cigar no cigar | winwin
    
    time - firstWinningN * 2 + 1
  }

  val result = solve(time, distance)
  println(s"Part 2 - result: $result")
}

@main def main() = {
  //part1
  part2
}
