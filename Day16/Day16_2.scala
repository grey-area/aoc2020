import io.Source
import annotation.tailrec


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

  private def potentialFieldIndicesInTicket(ticket: List[Int]) =
    ticket.zipWithIndex.filter{
      case (v, i) => contains(v)
    }.map(_._2).toSet

  def potentialFieldIndicesInTickets(tickets: List[List[Int]]) =
    tickets.map(potentialFieldIndicesInTicket).reduce(_ intersect _)
}


object Ticket {
  private def parseTicket(s: String) = s.split(',').map(_.toInt).toList
  def parseMyTicket(s: String) = parseTicket(s.split('\n')(1))
  def parseNearbyTickets(s: String) = s.split('\n').drop(1).map(parseTicket).toList

  private def isFieldValid(rules: Map[String, Rule], field: Int) =
    rules.values.exists {
      _ contains field
    }

  private def isTicketValid(rules: Map[String, Rule], ticket: List[Int]) = ticket.forall(isFieldValid(rules, _))

  def getValidTickets(rules: Map[String, Rule], tickets: List[List[Int]]) =
    tickets.filter(isTicketValid(rules, _))
}


object Day16 extends App {
  def parseInput(fname: String) = {
    val parts = Source.fromFile(fname).mkString.split("\n\n").toList
    val rules = Rule.parseRules(parts(0))
    val myTicket = Ticket.parseMyTicket(parts(1))
    val nearbyTickets = Ticket.parseNearbyTickets(parts(2))

    (rules, myTicket, nearbyTickets)
  }

  @tailrec
  def computeAssignments(unassigned: List[(String, Set[Int])], usedIndices: Set[Int] = Set.empty, assigned: Map[String, Int] = Map.empty): Map[String, Int] =
    unassigned match {
      case Nil => assigned
      case (name, indices) :: tail => {
        val index = (indices -- usedIndices).head
        computeAssignments(tail, usedIndices + index, assigned + (name -> index))
      }
    }

  val (rules, myTicket, nearbyTickets) = parseInput("inputs/input.txt")

  val validTickets = Ticket.getValidTickets(rules, nearbyTickets)
  val potentialAssignments = rules.map{
    case (k, r) => (k, r.potentialFieldIndicesInTickets(validTickets))
  }.toList.sortBy(_._2.size)

  val assignments = computeAssignments(potentialAssignments)
  val departureIndices = assignments.filter{
    case (k, v) => k.startsWith("departure")
  }.values

  val answer = departureIndices.map(idx => BigInt(myTicket(idx))).reduce(_ * _)
  println(answer)
}