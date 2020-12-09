import io.Source
import util.matching.Regex.Match
import annotation.tailrec


final case class Instruction(op: (Int, Computer) => Computer, arg: Int)


object Compiler {
  final case class UncompiledInstruction(opName: String, arg: Int)

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

  private def compileInstruction(inst: UncompiledInstruction) = {
    val op = inst.opName match {
      case "acc" => accOp
      case "jmp" => jmpOp
      case _ => nop
    }

    Instruction(op, inst.arg)
  }

  private def parseInstruction(m: Match) = {
    val opName = m.group(1)
    val arg = m.group(2).toInt

    UncompiledInstruction(opName, arg)
  }

  private def modifiedProgramAtIndex(program: Vector[UncompiledInstruction], idx: Int): Option[Vector[UncompiledInstruction]] = {
    val UncompiledInstruction(opName, arg) = program(idx)

    opName match {
      case "acc" => None
      case "jmp" => Some(program.updated(idx, UncompiledInstruction("nop", arg)))
      case "nop" => Some(program.updated(idx, UncompiledInstruction("jmp", arg)))
    }
  }

  private def compileProgram(program: Vector[UncompiledInstruction]) =
    program.map(compileInstruction)

  def apply(sourceCode: String) = {
    val pattern = """(?m)^([a-z]{3})\s([+-]\d+)$""".r
    val matches = pattern.findAllMatchIn(sourceCode)
    val uncompiledProgram = matches.map(parseInstruction).toVector

    val programCollection = (0 until uncompiledProgram.size).flatMap{
      modifiedProgramAtIndex(uncompiledProgram, _)
    }.map(compileProgram)

    programCollection.map{
      Computer(0, 0, _)
    }
  }
}


final case class Computer(ic: Int, acc: Int, program: Vector[Instruction]) {
  @tailrec
  def run(history: Set[Int] = Set[Int]()): Option[Int] = {
    if (ic == program.size)
      Some(acc)
    else if (history contains ic)
      None
    else {
      val Instruction(op, arg) = program(ic)
      op(arg, this).run(history + ic)
    }
  }
}


object Day08 extends App {
  val sourceCode = io.Source.fromFile("inputs/input.txt").mkString
  val computerCollection = Compiler(sourceCode)

  // Find a case where running the computer did not return None
  val answer = computerCollection.map(_.run()).find{
    ! _.isEmpty
  }.flatten

  answer match {
    case Some(v) => println(f"Terminated with value $v.")
    case None => println("Couldn't find a case where the computer terminated.")
  }
}
