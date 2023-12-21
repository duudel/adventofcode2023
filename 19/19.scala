//> using scala "3.3.1"
//> using jvm zulu:17

import scala.annotation.tailrec

def readInput() = {
  //val source = scala.io.Source.fromFile("input-example.txt")
  val source = scala.io.Source.fromFile("input.txt")
  val input = source.getLines().toSeq
  source.close()
  input
}

enum Target {
  case Rejected
  case Accepted
  case Workflow(name: String)
}

object Target {
  def parse(s: String): Target = s match {
    case "R"  => Target.Rejected
    case "A"  => Target.Accepted
    case name => Target.Workflow(name)
  }
}

type Rating = 'x' | 'm' | 'a' | 's'

object Rating {
  def fromString(s: String): Rating = s match {
    case "x" => 'x'
    case "m" => 'm'
    case "a" => 'a'
    case "s" => 's'
  }
}

case class Part(x: Int, m: Int, a: Int, s: Int) {
  def apply(rating: Rating): Int = rating match {
    case 'x' => x
    case 'm' => m
    case 'a' => a
    case 's' => s
  }
}

object Part {
  def parse(line: String): Part = line match {
    case s"{$ratings}" =>
      ratings
        .split(',')
        .foldLeft(Part(0,0,0,0)) {
          case (part, rating) =>
          rating match {
            case s"x=$value" => part.copy(x = value.toInt)
            case s"m=$value" => part.copy(m = value.toInt)
            case s"a=$value" => part.copy(a = value.toInt)
            case s"s=$value" => part.copy(s = value.toInt)
          }
        }
  }
}

enum Cond {
  case True
  case LessThan(rating: Rating, test: Int)
  case GreaterThan(rating: Rating, test: Int)

  def evaluate(part: Part): Boolean = this match {
    case True                       => true
    case LessThan(rating, test)     => part(rating) < test
    case GreaterThan(rating, test)  => part(rating) > test
  }
}

object Cond {
  def parse(s: String): Cond = s match {
    case s"$rating<$test" =>
      Cond.LessThan(Rating.fromString(rating), test.toInt)
    case s"$rating>$test" =>
      Cond.GreaterThan(Rating.fromString(rating), test.toInt)
  }
}

case class Rule(condition: Cond, target: Target) {
  def evaluate(part: Part): Option[Target] = {
    if (condition.evaluate(part)) {
      Some(target)
    } else None
  }
}

object Rule {
  def parse(s: String): Rule = s match {
    case s"$cond:$target" =>
      val c = Cond.parse(cond)
      val tgt = Target.parse(target)
      Rule(c, tgt)
    case plainTarget =>
      val tgt = Target.parse(plainTarget)
      Rule(Cond.True, tgt)
  }
}

case class Workflow(name: String, rules: Seq[Rule])

object Workflow {
  def parse(line: String): Workflow = {
    line match {
      case s"$name{$rules}" =>
        val rs = rules.split(',').map(Rule.parse)
        Workflow(name, rs.toVector)
    }
  }
}

case class Workflows(workflows: Map[String, Workflow]) {
  val in = workflows("in")
  def apply(name: String): Workflow = workflows(name)
}

def parseInput(lines: Seq[String]): (Workflows, Seq[Part]) = {
  val (workflowLines, partLines) = lines.span(line => !line.isBlank)
  val workflows = workflowLines.map(Workflow.parse)
  val parts = partLines.drop(1).map(Part.parse)
  (Workflows(workflows.map(w => w.name -> w).toMap), parts)
}

def part1 = {
  val (workflows, parts) = parseInput(readInput())

  def evaluate(workflows: Workflows, part: Part): Target = {
    @tailrec
    def evaluateRules(rule: Rule, rules: List[Rule]): Target = {
      rule.evaluate(part) match {
        case Some(target) =>
          //println(s"evaluate $rule :: -> $target")
          target
        case None =>
          //println(s"evaluate $rule :: -> no match")
          rules match {
          case r :: tail => evaluateRules(r, tail)
          case Nil => throw new MatchError("Invalid workflow")
        }
      }
    }

    @tailrec
    def impl(workflow: Workflow): Target = {
      //println(s"Part $part")
      val target = evaluateRules(workflow.rules.head, workflow.rules.toList.tail)
      target match {
        case Target.Rejected | Target.Accepted =>
          target
        case Target.Workflow(name) =>
          impl(workflows(name))
      }
    }
    
    impl(workflows.in)
  }

  val accepted = parts.foldLeft(Vector.empty[Part])(
    (accepted, part) => evaluate(workflows, part) match {
      case Target.Accepted => accepted :+ part
      case _               => accepted
    }
  )

  println("Accepted:")
  println(accepted)

  val result = accepted.map(p => p.x + p.m + p.a + p.s).sum

  println(s"Part 1 - $result")
}

case class State(x: Range, m: Range, a: Range, s: Range) {
  def combinations: Long = {
    if (x.start < x.end &&
        m.start < m.end &&
        a.start < a.end &&
        s.start < s.end) {
      x.size.toLong * m.size * a.size * s.size
    } else 0L
  }

  def lessThan(rating: Rating, test: Int): (State, State) = {
    rating match {
      case 'x' =>
        val lt = copy(x = x.start to (test - 1))
        val gtEq = copy(x = test to x.end) 
        (lt, gtEq)
      case 'm' =>
        val lt = copy(m = m.start to (test - 1))
        val gtEq = copy(m = test to m.end) 
        (lt, gtEq)
      case 'a' =>
        val lt = copy(a = a.start to (test - 1))
        val gtEq = copy(a = test to a.end) 
        (lt, gtEq)
      case 's' =>
        val lt = copy(s = s.start to (test - 1))
        val gtEq = copy(s = test to s.end) 
        (lt, gtEq)
    }
  }

  def greaterThan(rating: Rating, test: Int): (State, State) = {
    rating match {
      case 'x' =>
        val ltEq = copy(x = x.start to test)
        val gt = copy(x = (test + 1) to x.end) 
        (ltEq, gt)
      case 'm' =>
        val ltEq = copy(m = m.start to test)
        val gt = copy(m = (test + 1) to m.end) 
        (ltEq, gt)
      case 'a' =>
        val ltEq = copy(a = a.start to test)
        val gt = copy(a = (test + 1) to a.end) 
        (ltEq, gt)
      case 's' =>
        val ltEq = copy(s = s.start to test)
        val gt = copy(s = (test + 1) to s.end) 
        (ltEq, gt)
    }
  }
}

object State {
  def initial(min: Int, max: Int): State =
    State(min to max, min to max, min to max, min to max)
}

def part2 = {
  val (workflows, _) = parseInput(readInput())

  def countCombinations(workflows: Workflows): Long = {
    def evalTarget(target: Target, state: State, result: Long): Long = {
      if (state.combinations == 0) result
      else target match {
        case Target.Rejected       => result
        case Target.Accepted       => result + state.combinations
        case Target.Workflow(name) => eval(workflows(name), state, result)
      }
    }

    def evalRule(rule: Rule, rules: List[Rule], state: State, result: Long): Long = {
      if (state.combinations == 0) result
      else rule.condition match {
        case Cond.True => evalTarget(rule.target, state, result)
        case Cond.LessThan(rating, test) =>
          val (stateLt, stateGtEq) = state.lessThan(rating, test)
          val res = evalTarget(rule.target, stateLt, result)
          rules match {
            case h :: tail =>
              evalRule(h, tail, stateGtEq, res)
            case Nil => res
          }
        case Cond.GreaterThan(rating, test) =>
          val (stateLtEq, stateGt) = state.greaterThan(rating, test)
          val res = evalTarget(rule.target, stateGt, result)
          rules match {
            case h :: tail =>
              evalRule(h, tail, stateLtEq, res)
            case Nil => res
          }
      }
    }

    def eval(workflow: Workflow, state: State, result: Long): Long = {
      evalRule(workflow.rules.head, workflow.rules.toList.tail, state, result)
    }

    eval(workflows.in, State.initial(1, 4000), 0L)
  }

  val result = countCombinations(workflows)

  println(s"Part 2 - $result")
}

@main def main() = {
  //part1
  part2
}

