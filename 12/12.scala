//> using scala "3.3.1"
//> using jvm zulu:17

def readInput() = {
  val source = scala.io.Source.fromFile("input.txt")
  val input = source.getLines().toSeq
  source.close()
  input
}

enum Sym {
  case Operational
  case Damaged
  case Unknown

  override def toString(): String = this match {
    case Operational => "."
    case Damaged => "#"
    case Unknown => "?"
  }
}

def parse(lines: Seq[String]) = {
  lines.map { line =>
    line match {
      case s"$recordStr $countsStr" =>
        val record = recordStr.map {
          case '.' => Sym.Operational
          case '#' => Sym.Damaged
          case '?' => Sym.Unknown
        }
        val counts = countsStr.split(',').toIndexedSeq.map(_.toInt)
        (record, counts)
    }
  }
}

def part1 = {
  def damaged(sym: Sym, symbols: List[Sym], current: Int, counts: List[Int], record: String, solutions: Seq[String]): Seq[String] = {
    //println(s"damaged $sym ${symbols.mkString} : $current $counts")
    assert(current >= 0)
    sym match {
      case Sym.Operational if current == 0 =>
        solve(symbols, counts, record + ".", solutions)
      case Sym.Operational =>
        solutions // not enough damaged springs
      case Sym.Unknown if current == 0 =>
        // treat as operational
        solve(symbols, counts, record + ".", solutions)

      case Sym.Unknown if current >= 1 =>
        // treat as damaged
        damaged(Sym.Damaged, symbols, current, counts, record, solutions)

      case Sym.Damaged if current == 0 =>
        solutions // too long streak of damaged springs
      case Sym.Damaged =>
        symbols match {
          case h :: rest =>
            damaged(h, rest, current - 1, counts, record + "#", solutions)
          case Nil if current == 1 =>
            solve(Nil, counts, record + "#", solutions)
          case Nil =>
            solutions
        }

      case Sym.Unknown => ??? // to make match cases complete and appease the compiler
    }
  }

  def initial(sym: Sym, symbols: List[Sym], counts: List[Int], record: String, solutions: Seq[String]): Seq[String] = {
    //println(s"initial $sym ${symbols.mkString} : $counts")
    sym match {
      case Sym.Operational =>
        solve(symbols, counts, record + ".", solutions)
      case Sym.Damaged =>
        counts match {
          case current :: countsTail =>
            damaged(Sym.Damaged, symbols, current, countsTail, record, solutions)
          case Nil =>
            solutions
        }
      case Sym.Unknown =>
        initial(Sym.Operational, symbols, counts, record, solutions)
        ++ initial(Sym.Damaged, symbols, counts, record, solutions)
    }
  }

  def solve(symbols: List[Sym], counts: List[Int], record: String, solutions: Seq[String]): Seq[String] = {
    symbols match {
      case h :: tail              => initial(h, tail, counts, record, solutions)
      case Nil if counts.isEmpty  => solutions :+ record
      case Nil                    => solutions
    }
  }

  val parsed = parse(readInput())
  val solutions = parsed.map((record, counts) => solve(record.toList, counts.toList, "", Seq.empty))

  println(solutions.mkString("\n"))
  println(solutions.map(_.size))

  val result = solutions.map(_.size).sum
  println(s"Part 1 - result: $result")
}

def part2 = {
  type N = Long
  val memo = scala.collection.mutable.OpenHashMap.empty[(String, String), N]

  def damaged(sym: Sym, symbols: List[Sym], current: Int, counts: List[Int], result: N): N = {
    assert(current >= 0)
    sym match {
      case Sym.Operational if current == 0 =>
        solve(symbols, counts, result)
      case Sym.Operational =>
        result // not enough damaged springs
      case Sym.Unknown if current == 0 =>
        // treat as operational
        solve(symbols, counts, result)

      case Sym.Unknown if current >= 1 =>
        // treat as damaged
        damaged(Sym.Damaged, symbols, current, counts, result)

      case Sym.Damaged if current == 0 =>
        result // too long streak of damaged springs
      case Sym.Damaged =>
        symbols match {
          case h :: rest =>
            damaged(h, rest, current - 1, counts, result)
          case Nil if current == 1 =>
            solve(Nil, counts, result)
          case Nil =>
            result
        }

      case Sym.Unknown => ??? // to make match cases complete and appease the compiler
    }
  }

  def initial(sym: Sym, symbols: List[Sym], counts: List[Int], result: N): N = {
    sym match {
      case Sym.Operational =>
        solve(symbols, counts, result)
      case Sym.Damaged =>
        counts match {
          case current :: countsTail =>
            damaged(Sym.Damaged, symbols, current, countsTail, result)
          case Nil =>
            result
        }
      case Sym.Unknown =>
        initial(Sym.Operational, symbols, counts, result)
        + initial(Sym.Damaged, symbols, counts, result)
    }
  }

  def memoKey(symbols: List[Sym], counts: List[Int]): (String, String) = {
    val symKey = symbols.mkString
    val countKey = counts.mkString(",")
    (symKey, countKey)
  }

  def solve(symbols: List[Sym], counts: List[Int], result: N): N = {
    symbols match {
      case Nil if counts.isEmpty  => result + 1
      case Nil                    => result
      case h :: tail              =>
        val key = memoKey(symbols, counts)
        memo.get(key) match {
          case Some(res) => result + res
          case None =>
            val res = initial(h, tail, counts, result)
            memo.put(key, res)
            res
        }
    }
  }

  def unfold(item: (Seq[Sym], Seq[Int])): (Seq[Sym], Seq[Int]) = {
    val (r, c) = item
    val record = Seq.fill(5)(r).flatMap(r => r :+ Sym.Unknown).dropRight(1)
    val counts = Seq.fill(5)(c).flatten
    (record, counts)
  }

  val parsed = parse(readInput()).map(unfold)
  val solutions = parsed
    .zipWithIndex
    .map { case ((record, counts), index) =>
      val num = solve(record.toList, counts.toList, 0)
      println(s"$index: $num solutions")
      num
    }

  val result = solutions.map(_.toLong).sum
  println(s"Part 2 - result: $result")
}

@main def main() = {
  //part1
  part2
}
