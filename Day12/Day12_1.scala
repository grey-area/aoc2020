import io.Source
import util.matching.Regex.Match


final case class Instruction(instructionType: String, value: Int)

object Ship {
  val directions = Vector("N", "E", "S", "W")

  private def unpackInstruction(m: Match) =
    Instruction(m.group(1), m.group(2).toInt)

  def parseInstructions(instructions: String) = {
    val regex = """(?m)^([NESWFLR])(\d+)$""".r
    regex.findAllMatchIn(instructions).map(unpackInstruction).toList
  }

  def manhattanDistance(s1: Ship, s2: Ship) =
    (s1.x - s2.x).abs + (s1.y - s2.y).abs
}

final case class Ship(x: Int = 0, y: Int = 0, direction: Int = 1) {
  private def moveNorth(amount: Int) = Ship(x, y + amount, direction)
  private def moveEast(amount: Int) = Ship(x + amount, y, direction)
  private def turnRight(amount: Int) = Ship(x, y, (direction + amount) % 4)
  private def moveForwards(amount: Int): Ship =
    followInstruction(Instruction(Ship.directions(direction), amount))

  private def followInstruction(inst: Instruction): Ship =
    inst.instructionType match {
      case "N" => moveNorth(inst.value)
      case "S" => moveNorth(-inst.value)
      case "E" => moveEast(inst.value)
      case "W" => moveEast(-inst.value)
      case "R" => turnRight(inst.value / 90)
      case "L" => turnRight(4 - inst.value / 90)
      case "F" => moveForwards(inst.value)
    }

  def update(instructions: List[Instruction]): Ship =
    instructions match {
      case Nil => this
      case inst :: tail => {
        val newShip = followInstruction(inst)
        newShip.update(tail)
      }
    }
}


object Day12 extends App {
  val instructions = Ship.parseInstructions(Source.fromFile("inputs/input.txt").mkString)

  val initialPosition = Ship()
  val finalPosition = initialPosition.update(instructions)
  val answer = Ship.manhattanDistance(initialPosition, finalPosition)

  println(answer)
}