//> using scala "3.3.1"
//> using jvm zulu:17

import scala.annotation.tailrec

def readInput(filename: String) = {
  val source = scala.io.Source.fromFile(filename)
  val input = source.getLines().toSeq
  source.close()
  input
}

enum Pulse {
  case high, low
}

case class PulseItem(pulse: Pulse, from: ModuleName, to: ModuleName)

case class State[T](var value: T) {
  def mod(f: T => T): State[T] = {
    value = f(value)
    return this;
  }
  def set(newValue: T): State[T] = {
    value = newValue
    return this;
  }
}

type ModuleName = String

enum Module(val name: ModuleName, val targets: Seq[ModuleName]) {
  case Broadcaster(
    targets0: Seq[ModuleName]
  ) extends Module("broadcaster", targets0)
  case FlipFlop(
    name0: ModuleName, on: State[Boolean], targets0: Seq[ModuleName]
  ) extends Module(name0, targets0)
  case Conjunction(
    name0: ModuleName, inputs: State[Map[ModuleName, Pulse]], targets0: Seq[ModuleName]
  ) extends Module(name0, targets0)
}

def parseInput(lines: Seq[String]): Modules = {
  def parseNameAndTargets(line: String): (ModuleName, Seq[ModuleName]) = {
    val parts = line.split(" -> ")
    val name = parts.head
    val targets = parts(1).split(", ")
    (name, targets.toSeq)
  }

  var broadcaster: Module.Broadcaster = null
  val modules = lines.map { line =>
    if (line.startsWith("broadcaster")) {
      val (_, targets) = parseNameAndTargets(line)
      broadcaster = Module.Broadcaster(targets)
      broadcaster
    } else {
      line.head match {
        case '%' =>
          val (name, targets) = parseNameAndTargets(line.drop(1))
          Module.FlipFlop(name, State(false), targets)
        case '&' =>
          val (name, targets) = parseNameAndTargets(line.drop(1))
          Module.Conjunction(name, State(Map.empty), targets)
      }
    }
  }
  
  val modulesMap = modules.map(m => m.name -> m).toMap

  modules.foreach { module =>
    module.targets.foreach { target =>
      modulesMap.get(target) match {
        case Some(Module.Conjunction(_, inputs, _)) =>
          inputs.mod(_.updated(module.name, Pulse.low))
        case _ => ()
      }
    }
  }

  new Modules(broadcaster, modulesMap)
}

class Modules(
  broadcaster: Module.Broadcaster,
  modules: Map[ModuleName, Module]
) {
  private var pulseProbe: PulseItem => Unit = null

  def installProbe(probe: PulseItem => Unit): Unit = {
    pulseProbe = probe
  }

  val pulses = scala.collection.mutable.Queue.empty[PulseItem]

  def emitPulse(pulse: PulseItem): Unit = {
    //println(s"pulse: $pulse")
    if (pulseProbe != null) pulseProbe(pulse)
    pulses.enqueue(pulse)
  }

  def emitPulses(pulse: Pulse, from: ModuleName, targets: Seq[ModuleName]): Unit = {
    targets.foreach(to => emitPulse(PulseItem(pulse, from, to)))
  }

  def pushButton(): Unit = {
    emitPulses(Pulse.low, "button", Seq(broadcaster.name))
  }

  def run(): Unit = {
    while (pulses.nonEmpty) {
      val PulseItem(pulse, from, to) = pulses.dequeue()
      modules.get(to).foreach { module =>
        module match {
          case Module.Broadcaster(targets) =>
            emitPulses(pulse, module.name, targets)
          case Module.FlipFlop(name, on, targets) =>
            if (pulse == Pulse.low) {
              val newPulse = if (on.value) Pulse.low else Pulse.high
              on.mod(v => !v)
              emitPulses(newPulse, name, targets)
            }
          case Module.Conjunction(name, inputs, targets) =>
            inputs.mod(_.updated(from, pulse))
            val allHigh = inputs.value.forall(_._2 == Pulse.high)
            val newPulse = if (allHigh) Pulse.low else Pulse.high
            emitPulses(newPulse, name, targets)
        }
      }
    }
  }

  def modulesThatTarget(target: ModuleName): Seq[Module] = {
    modules.values.filter(_.targets.contains(target)).toSeq
  }
  
  def findLowestCommonMultipleOfPeriods(): BigInt = {
    val rxParentsParents = modulesThatTarget("rx")
      .flatMap(m => modulesThatTarget(m.name))

    println(s"rx <- () <-: ${rxParentsParents.mkString(",")}")

    var periods = rxParentsParents.map(m => m.name -> 0L).toMap
    var buttonPresses = 0L

    val probe: PulseItem => Unit = pulse => {
      if (pulse.pulse == Pulse.high) {
        periods.get(pulse.from) match {
          case Some(n) if n == 0 =>
            println(s"Period ${pulse.from} at $buttonPresses presses")
            periods = periods + (pulse.from -> buttonPresses)
          case _ => ()
        }
      }
    }
    installProbe(probe)

    var done = false
    while (!done) {
      buttonPresses += 1
      pushButton()
      run()

      done = periods.values.forall(_ != 0)
    }
    
    def lcm(a: BigInt, b: BigInt): BigInt = {
      return (a * b) / a.gcd(b);
    }

    periods.values.map(BigInt(_)).reduce((a, b) => lcm(a, b))
  }
}

class CountProbe() {
  var high: Long = 0
  var low: Long = 0

  def reset: Unit = {
    high = 0
    low = 0
  }

  def probe: PulseItem => Unit = { pulse =>
    if (pulse.pulse == Pulse.high) {
      high += 1
    } else {
      low += 1
    }
  }
}

object Test {
  def apply(name: String)(f: => Unit): Unit = {
    try {
      f
    } catch {
      case t: Throwable =>
        println(s"Test [$name] failed")
        t.printStackTrace()
    }
  }

  def assertEquals[A](expected: A, received: A): Unit = {
      if (expected != received) {
        throw new Exception(s"expected $expected, received $received")
      }
  }
}

def tests = {
  def confirmPulses(pulses: Seq[PulseItem]): PulseItem => Unit = {
    var iterator = pulses.iterator
    (item: PulseItem) => {
      val n = iterator.next()
      Test.assertEquals(n, item)
    }
  }

  val expectedPulses0 = Seq(
    PulseItem(Pulse.low, "button", "broadcaster"),
    PulseItem(Pulse.low, "broadcaster", "a"),
    PulseItem(Pulse.low, "broadcaster", "b"),
    PulseItem(Pulse.low, "broadcaster", "c"),
    PulseItem(Pulse.high, "a", "b"),
    PulseItem(Pulse.high, "b", "c"),
    PulseItem(Pulse.high, "c", "inv"),
    PulseItem(Pulse.low, "inv", "a"),
    PulseItem(Pulse.low, "a", "b"),
    PulseItem(Pulse.low, "b", "c"),
    PulseItem(Pulse.low, "c", "inv"),
    PulseItem(Pulse.high, "inv", "a"),
  )
  
  Test("hand-built modules") {
    // broadcaster -> a, b, c
    // %a -> b
    // %b -> c
    // %c -> inv
    // &inv -> a

    val broadcaster: Module.Broadcaster = Module.Broadcaster(Seq("a", "b", "c"))
    val a = Module.FlipFlop("a", State(false), Seq("b"))
    val b = Module.FlipFlop("b", State(false), Seq("c"))
    val c = Module.FlipFlop("c", State(false), Seq("inv"))
    val inv = Module.Conjunction("inv", State(Map("c" -> Pulse.low)), Seq("a"))

    val modules = new Modules(
      broadcaster,
      Seq(broadcaster, a, b, c, inv).map(m => m.name -> m).toMap
    )

    modules.installProbe(confirmPulses(expectedPulses0))

    modules.pushButton()
    modules.run()
    
    modules.installProbe(confirmPulses(expectedPulses0))

    modules.pushButton()
    modules.run()
  }
  
  Test("parsed simple modules") {
    val modules = parseInput(readInput("input-example.txt"))

    modules.installProbe(confirmPulses(expectedPulses0))

    modules.pushButton()
    modules.run()
    
    modules.installProbe(confirmPulses(expectedPulses0))

    modules.pushButton()
    modules.run()

    val count = new CountProbe()
    modules.installProbe(count.probe)

    modules.pushButton()
    modules.run()
    Test.assertEquals(8, count.low)
    Test.assertEquals(4, count.high)

    count.reset
    (1 to 1000).foreach { _ =>
      modules.pushButton()
      modules.run()
    }
    
    Test.assertEquals(8000, count.low)
    Test.assertEquals(4000, count.high)
  }
  
  Test("parsed simple modules - 2") {
    val modules = parseInput(readInput("input-example2.txt"))

    val count = new CountProbe()
    modules.installProbe(count.probe)

    (1 to 1000).foreach(_ => {
      modules.pushButton()
      modules.run()
    })

    Test.assertEquals(4250, count.low)
    Test.assertEquals(2750, count.high)
  }
}

def part1 = {
  val modules = parseInput(readInput("input.txt"))
  
  val count = CountProbe()
  modules.installProbe(count.probe)

  (1 to 1000).foreach(_ => {
    modules.pushButton()
    modules.run()
  })

  val result = count.low * count.high
  println(s"Part 1 - $result")
}

def part2 = {
  val modules = parseInput(readInput("input.txt"))

  // Too slow
  //
  //var rxLowReceived = false
  //val probe: PulseItem => Unit = { pulse =>
  //  if (pulse.pulse == Pulse.low && pulse.to == "rx") {
  //    rxLowReceived = true
  //  }
  //}
  //
  //modules.installProbe(probe)

  //var buttonPresses: Long = 0
  //while (!rxLowReceived) {
  //  modules.pushButton()
  //  modules.run()
  //  buttonPresses += 1
  //}

  //println(s"Part 2 - $buttonPresses")
  
  val result = modules.findLowestCommonMultipleOfPeriods();
  println(s"Part 2 - $result")
}

@main def main() = {
  tests
  part1
  part2
}

