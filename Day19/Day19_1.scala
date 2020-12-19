object Rule {
  def apply(pairs: List[(String, String)]) = {
    val ruleMap = Map(pairs: _*)
    new Rule(ruleMap)
  }
}

final class Rule(private val ruleMap: Map[String, String]) {
  private val Terminal = """"([a-z])"""".r
  private val Disjunction = """(.+) \| (.+)""".r

  private def memoize[K, V](f: K => V): K => V = {
    val cache = collection.mutable.Map.empty[K, V]
    k => cache.getOrElseUpdate(k, f(k))
  }

  private def split(keys: String) =
    keys.trim.split(' ').map(get).mkString

  private def _get(key: String): String = ruleMap(key) match {
    case Terminal(v) => v
    case Disjunction(a, b) => f"(?:${split(a)}|${split(b)})"
    case s => split(s)
  }
  private val get = memoize(_get)

  private val pattern = get("0").r

  def matches(message: String) = pattern.matches(message)
}


object Parser {
  val leftRightPattern = """^(\d+): (.+)$""".r

  private def parseRule(rule: String) = rule match {
    case leftRightPattern(key, value) => key -> value
  }

  def parseInput(fname: String) = {
    val Array(ruleString, messagesString) = io.Source.fromFile(fname).mkString.split("\n\n")

    val rule = Rule(ruleString.split('\n').map(parseRule).toList)
    val messages = messagesString.split('\n')

    (rule, messages)
  }
}


object Day19 extends App {
  val (rule, messages) = Parser.parseInput("inputs/input.txt")

  val answer = messages.count(rule.matches(_))
  println(answer)
}