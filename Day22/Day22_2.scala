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
  def play(history: Set[Game] = Set.empty): (Int, Vector[Int]) =
    if (history.contains(this) || hands(1).isEmpty)
      (0, hands(0))
    else if (hands(0).isEmpty)
      (1, hands(1))
    else hands match {
      case (hd1 +: tl1) :: (hd2 +: tl2) :: Nil => {
        val roundWinnerID =
          if (tl1.size >= hd1 && tl2.size >= hd2) {
            val (winnerID, _) = Game(List(tl1.take(hd1), tl2.take(hd2))).play()
            winnerID
          }
          else
            if (hd1 > hd2) 0 else 1
        val heads = List(hands(roundWinnerID).head, hands(1 - roundWinnerID).head)
        val newHands =
          if (roundWinnerID == 0)
            List(tl1 ++ heads, tl2)
          else
            List(tl1, tl2 ++ heads)
        Game(newHands).play(history + this)
      }
    }
}


object Day22 extends App {
  val text = io.Source.fromFile("inputs/input.txt").mkString

  val game = Game.parseText(text)
  val (_, winningHand) = game.play()
  val answer = Game.computeScore(winningHand)
  println(answer)
}