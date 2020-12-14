import io.Source


object Mask {
  private def parseBinary(bin: String) = {
    val upper = BigInt(Integer.parseInt(bin.slice(0, 18), 2))
    val lower = BigInt(Integer.parseInt(bin.slice(18, 36), 2))
    (upper << 18) + lower
  }

  def empty = Mask("X" * 36)

  def apply(maskString: String) = {
    val careMask = parseBinary(maskString.replace('0', '1').replace('X', '0'))
    val valueMask = parseBinary(maskString.replace('X', '0'))
    new Mask(careMask, valueMask)
  }
}

final class Mask(val careMask: BigInt, val valueMask: BigInt) {
  def apply(value: BigInt) =
    (value & ~careMask) | valueMask
}


object MemoryProcessor {
  val maskRegex = """^mask = ([01X]{36})$""".r
  val memRegex = """^mem\[(\d+)\] = (\d+)$""".r
}

final case class MemoryProcessor(memory: Map[Int, BigInt] = Map.empty, mask: Mask = Mask.empty) {
  private def processLine(line: String) = line match {
    case MemoryProcessor.maskRegex(maskString) => {
      val newMask = Mask(maskString)
      MemoryProcessor(memory, newMask)
    }
    case MemoryProcessor.memRegex(addr, value) => {
      MemoryProcessor(
        memory + (addr.toInt -> mask(BigInt(value))),
        mask
      )
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