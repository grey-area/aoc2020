import io.Source


object Rule {
  private val pattern = """^([\w\s]+): (\d+)-(\d+) or (\d+)-(\d+)$""".r

  private def parseRule(s: String) = s match {
    case pattern(name, l1, u1, l2, u2) =>
      (name -> new Rule(l1.toInt to u1.toInt, l2.toInt to u2.toInt))
    }

  def parseRules(s: String) = Map(s.split('\n').map(parseRule).toIndexedSeq: _*)
}

final class Rule(private val r1: Range, private val r2: Range) {
  def contains(v: Int) = (r1 contains v) || (r2 contains v)
}


object Ticket {
  private def parseTicket(s: String) = s.split(',').map(_.toInt).toList
  def parseMyTicket(s: String) = parseTicket(s.split('\n')(1))
  def parseNearbyTickets(s: String) = s.split('\n').drop(1).map(parseTicket).toList
}


object Day16 extends App {
  def parseInput(fname: String) = {
    val parts = Source.fromFile(fname).mkString.split("\n\n").toList
    val rules = Rule.parseRules(parts(0))
    val nearbyTickets = Ticket.parseNearbyTickets(parts(2))

    (rules, nearbyTickets)
  }

  def isFieldValid(rules: Map[String, Rule], field: Int) =
    rules.values.exists {
      _ contains field
    }

  def findInvalidFields(rules: Map[String, Rule], ticket: List[Int]) = ticket.filter(! isFieldValid(rules, _))

  def findInvalid(rules: Map[String, Rule], tickets: List[List[Int]]) =
    tickets.flatMap(findInvalidFields(rules, _))

  val (rules, nearbyTickets) = parseInput("inputs/input.txt")
  val invalidFieldsSum = findInvalid(rules, nearbyTickets).sum
  println(invalidFieldsSum)
}