import io.Source


object Mask {
  private def parseBinary(bin: String) = {
    val upper = BigInt(Integer.parseInt(bin.slice(0, 18), 2))
    val lower = BigInt(Integer.parseInt(bin.slice(18, 36), 2))
    (upper << 18) + lower
  }

  def floatBit(addr: BigInt, wildIdx: Int) = {
    val flipBit = BigInt(1) << wildIdx
    List(addr, addr ^ flipBit)
  }

  def empty = Mask("X" * 36)

  def apply(maskString: String) = {
    val oneMask = parseBinary(maskString.replace('X', '0'))
    val wildIdx = maskString.zipWithIndex.collect { case (x, i) if x=='X' => 35 - i }
    new Mask(oneMask, wildIdx.toList)
  }
}

final class Mask(val oneMask: BigInt, val wildIdx: List[Int]) {
  private def allAddr(addr: BigInt, wildIdx: List[Int]): List[BigInt] = wildIdx match {
    case Nil => List(addr)
    case head :: tail => Mask.floatBit(addr, head).flatMap(allAddr(_, tail))
  }

  def apply(memory: Map[BigInt, BigInt], addr: BigInt, value: BigInt) = {
    val updateAddresses = allAddr(addr | oneMask, wildIdx)

    updateAddresses.foldLeft(memory) {
      (mem, idx) => mem + (idx -> value)
    }
  }
}


object MemoryProcessor {
  val maskRegex = """^mask = ([01X]{36})$""".r
  val memRegex = """^mem\[(\d+)\] = (\d+)$""".r
}

final case class MemoryProcessor(memory: Map[BigInt, BigInt] = Map.empty, mask: Mask = Mask.empty) {
  private def processLine(line: String) = line match {
    case MemoryProcessor.maskRegex(maskString) => {
      val newMask = Mask(maskString)
      MemoryProcessor(memory, newMask)
    }
    case MemoryProcessor.memRegex(addr, value) => {
      val newMemory = mask(memory, BigInt(addr), BigInt(value))
      MemoryProcessor(newMemory, mask)
    }
  }

  def processLines(lines: List[String]): BigInt =
    lines match {
      case Nil => memory.values.sum
      case head :: tail => processLine(head).processLines(tail)
    }
}


object Day14 extends App {
  val lines = Source.fromFile("inputs/input.txt").getLines().toList
  val memProcessor = MemoryProcessor()

  val answer = memProcessor.processLines(lines)
  println(answer)
}