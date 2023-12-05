//> using scala "3.3.0"
//> using jvm zulu:17

import scala.collection.mutable
import scala.annotation.tailrec

def readInput(): Seq[String] = {
  val source = scala.io.Source.fromFile("input.txt")
  val input = source.getLines().toSeq
  source.close()
  input
}

case class Mapping(destStart: Long, sourceStart: Long, length: Long) {
  def intersects(start: Long, end: Long): Boolean =
    (start < sourceStart + length && end > sourceStart) ||
    (start >= sourceStart && end <= sourceStart + length)

  // Intersects start-end with the mapping's source range.
  // Maps overlapping ranges by adding the difference between
  // source and destination. Non-overlapping ranges are returned as is.
  // Remaining range is returned in the right side of the pair.
  //
  def clipMap(start: Long, end: Long): (Seq[(Long, Long)], (Long, Long)) = {
    val sourceEnd = sourceStart + length
    val diff = destStart - sourceStart
    val ranges = Seq(
      (start, sourceStart),
      (sourceStart.max(start) + diff, sourceEnd.min(end) + diff),
    ).filter((s, e) => s < e)

    val rest = (sourceEnd.max(start), end)

    ranges -> rest
  }
}

case class Maps(m: mutable.Buffer[Mapping]) {
  def lookup(n: Long): Long = {
    m.find(mapping => mapping.sourceStart <= n && n < mapping.sourceStart + mapping.length)
      .map(mapping => n - mapping.sourceStart + mapping.destStart)
      .getOrElse(n)
  }

  def sorted: Maps = Maps(m.sortBy(_.sourceStart))

  def clip(s1: Long, s2: Long): Seq[(Long, Long)] = {
    val (ranges, rest) = m.foldLeft((Seq.empty[(Long, Long)], (s1, s2))) {
      case ((result, (start, end)), mapping) =>
        val (ranges, rest) = mapping.clipMap(start, end)
        (result ++ ranges, rest)
    }

    (ranges :+ rest).filter((s, e) => s < e)
  }
}

case class Almanac(seeds: Seq[Long], maps: Seq[Maps]) {
  def sortedMaps: Almanac = Almanac(seeds, maps.map(_.sorted))
}

object Almanac {
  val empty: Almanac = Almanac(Seq.empty, Seq.empty)

  def parse(input: Seq[String]): Almanac = {
    input.foldLeft(Almanac.empty) {
      case (Almanac(Seq(), _), line) => line match {
        case "" => Almanac.empty
        case s"seeds: $seeds" =>
          val s = seeds.split(' ').toIndexedSeq.map(_.toLong)
          Almanac(s, Seq.empty)
      }
      case (almanac, "") => almanac
      case (almanac, line) => line match {
        case s"$category map:" =>
          almanac.copy(maps = almanac.maps :+ Maps(mutable.Buffer.empty))
        case s"$d $s $len" =>
          val currentMap = almanac.maps.last
          currentMap.m += Mapping(d.toLong, s.toLong, len.toLong)
          almanac
      }
    }.sortedMaps
  }
}

def part1(input: Seq[String]) = {
  val almanac = Almanac.parse(input)

  def doMapping(maps: List[Maps], seed: Long) = {
    def impl(n: Long, head: Maps, tail: List[Maps]): Long = {
      val result = head.lookup(n)
      tail match {
        case Nil => result
        case h :: rest => impl(result, h, rest)
      }
    }

    maps match {
      case head :: tail =>
        impl(seed, head, tail)
      case Nil =>
        throw new MatchError("Empty List of Maps!")
    }
  }

  val maps = almanac.maps.toList
  val seedDests = almanac.seeds.map(doMapping(maps, _))

  val result = seedDests.min

  println(s"Part 1 - result: $result")
}

def part2(input: Seq[String]) = {
  val almanac = Almanac.parse(input)

  def doMapping(maps: List[Maps], seeds: List[Long]): Long = {
    val (seedStarts, seedLengths) = seeds.zipWithIndex.partition((_, index) => (index & 1) == 0)
    val seedPairs = seedStarts.map(_._1).zip(seedLengths.map(_._1))
      .map((s, len) => (s, s + len))
    println(seedPairs)

    def implRange(s1: Long, s2: Long, maps: List[Maps]): Long = {
      //println(s"Maps ${maps.size}: $s1 $s2")
      maps match {
        case Nil =>
          println(s"Touch grass $s1")
          s1
        case m :: tail =>
          val ranges = m.clip(s1, s2)
          //println(s"Clipped to: $ranges")
          implRanges(ranges.toList, tail)
      }
    }

    def implRanges(ranges: List[(Long, Long)], maps: List[Maps]): Long = {
      ranges match {
        case Nil =>
          throw new MatchError("Wot!")
        case (s1, s2) :: tail =>
          val candidate = implRange(s1, s2, maps)
          tail match {
            case Nil => candidate
            case some => implRanges(some, maps).min(candidate)
          }
      }
    }

    implRanges(seedPairs, maps)
  }

  val maps = almanac.maps.toList
  val seeds = almanac.seeds.toList
  val result = doMapping(maps, seeds)
  
  println(s"Part 2 - result: $result")
}

@main def main() = {
  val input = readInput()
  //part1(input)
  part2(input)
}

