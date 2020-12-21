object AllergenAssignment {
  private val Pattern = """^(.+) \(contains (.+)\)$""".r

  private def parseLine(line: String) = line match {
    case Pattern(lhs, rhs) => {
      val ingredients = lhs.split(' ').map(_.trim).toList
      val allergens = rhs.split(',').map(_.trim).toList
      (allergens, ingredients)
    }
  }

  private def buildMap(lines: List[(List[String], List[String])], allergenMap: Map[String, Set[String]] = Map.empty): Map[String, Set[String]] = lines match {
    case Nil => allergenMap
    case (allergens, ingredients) :: tail => {
      val ingredientSet = ingredients.toSet
      val newMap = Map(allergens zip allergens.map{
        allergen => if (allergenMap contains allergen) allergenMap(allergen) & ingredientSet else ingredientSet
      }: _*)
      buildMap(tail, allergenMap ++ newMap)
    }
  }

  def parseInput(fname: String) = {
    val lines = io.Source.fromFile(fname).getLines().map(parseLine).toList
    val allIngredients = lines.map(_._2).reduce(_ ++ _)
    val allergenMap = buildMap(lines)
    (allergenMap, allIngredients)
  }
}


object Day21 extends App {
  def assign(unassignedAllergens: Map[String, Set[String]], assignedAllergens: Map[String, String] = Map.empty): Map[String, String] =
    if (unassignedAllergens.isEmpty)
      assignedAllergens
    else {
      val (allergen, ingredient) = unassignedAllergens.find(_._2.size == 1).get
      val newUnassignedAllergens = unassignedAllergens.map{
        case (k, v) => (k, v -- ingredient)
      } - allergen
      val newAssignedAllergens = assignedAllergens + (allergen -> ingredient.head)
      assign(newUnassignedAllergens, newAssignedAllergens)
    }

  val (allergenMap, allIngredients) = AllergenAssignment.parseInput("inputs/input.txt")
  val assignedAllergens = assign(allergenMap)

  val usedIngredients = assignedAllergens.values.toSet
  val answer1 = allIngredients.filterNot(usedIngredients contains _).size
  println(f"Answer for part 1: $answer1")

  val answer2 = assignedAllergens.keys.toList.sorted.map(assignedAllergens(_)).mkString(",")
  println(f"Answer for part 2: $answer2")
}