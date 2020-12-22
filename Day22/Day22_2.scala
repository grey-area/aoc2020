object Game {
  private def parsePlayer(s: String) = s.split('\n').drop(1).map(_.toInt).toVector

  def parseText(s: String) = {
    val playerHands = s.split("\n\n") map parsePlayer
    Game(playerHands.toList)
  }

  def computeScore(hand: Vector[Int]): Long = {
    val N = hand.size
    (hand.reverse zip (1 to N)).map(p => p._1 * p._2).sum
  }
}

final case class Game(hands: List[Vector[Int]]) {
  private def makeNewHands(roundWinnerID: Int) = {
    val heads = List(hands(roundWinnerID).head, hands(1 - roundWinnerID).head)
    val tails = hands.map(_.tail)
    tails.updated(roundWinnerID, tails(roundWinnerID) ++ heads)
  }

  private def computeRoundWinner(hd1: Int, hd2: Int, tl1: Vector[Int], tl2: Vector[Int]) = {
    if (tl1.size >= hd1 && tl2.size >= hd2)
      Game(List(tl1.take(hd1), tl2.take(hd2))).playRecursive()._1
    else if (hd1 > hd2) 0 else 1
  }

  private def playRecursive(history: Set[Game] = Set.empty): (Int, Vector[Int]) =
    if (history.contains(this) || hands(1).isEmpty)
      (0, hands(0))
    else if (hands(0).isEmpty)
      (1, hands(1))
    else hands match {
      case (hd1 +: tl1) :: (hd2 +: tl2) :: Nil => {
        val roundWinnerID = computeRoundWinner(hd1, hd2, tl1, tl2)
        val newHands = makeNewHands(roundWinnerID)
        Game(newHands).playRecursive(history + this)
      }
    }

  def play() = playRecursive()._2
}


object Day22 extends App {
  val text = io.Source.fromFile("inputs/input.txt").mkString

  val game = Game.parseText(text)
  val winningHand = game.play()
  val answer = Game.computeScore(winningHand)
  println(answer)
}