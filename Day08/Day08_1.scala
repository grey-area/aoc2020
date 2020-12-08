import io.Source
import util.matching.Regex.Match
import annotation.tailrec


final case class Instruction(op: (Int, Computer) => Computer, arg: Int)


object Compiler {
  private val accOp = (arg: Int, computer: Computer) => {
    val Computer(ic, acc, program) = computer
    Computer(ic + 1, acc + arg, program)
  }
  private val jmpOp = (arg: Int, computer: Computer) => {
    val Computer(ic, acc, program) = computer
    Computer(ic + arg, acc, program)
  }
  private val nop = (arg: Int, computer: Computer) => {
    val Computer(ic, acc, program) = computer
    Computer(ic + 1, acc, program)
  }

  private def parseMatch(m: Match) = {
    val opName = m.group(1)
    val arg = m.group(2).toInt
    val op = opName match {
      case "acc" => accOp
      case "jmp" => jmpOp
      case _ => nop
    }

    Instruction(op, arg)
  }

  def apply(sourceCode: String) = {
    val pattern = """(?m)^([a-z]{3})\s([+-]\d+)$""".r
    val matches = pattern.findAllMatchIn(sourceCode)
    val program = matches.map(parseMatch).toVector

    Computer(ic = 0, acc = 0, program = program)
  }
}


final case class Computer(ic: Int, acc: Int, program: Vector[Instruction]) {
  def run(history: Set[Int] = Set[Int]()): Int = {
    if (history contains ic) acc else {
      val Instruction(op, arg) = program(ic)
      op(arg, this).run(history + ic)
    }
  }
}


object Day08 extends App {
  val sourceCode = io.Source.fromFile("inputs/input.txt").mkString
  val computer = Compiler(sourceCode)

  val answer = computer.run()
  println(answer)
}
