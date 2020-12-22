object Game {
  private def parsePlayer(s: String) = s.split('\n').drop(1).map(_.toInt).toVector

  def parseText(s: String) = {
    val playerHands = s.split("\n\n") map parsePlayer
    Game(playerHands(0), playerHands(1))
  }

  private def computeScore(hand: Vector[Int]): Long = {
    val N = hand.size
    (hand.reverse zip (1 to N)).map(p => p._1 * p._2).sum
  }
}

final case class Game(hand1: Vector[Int], hand2: Vector[Int]) {
  def play(): Long = {
    val List(shortHand, longHand) = List(hand1, hand2).sortWith(_.size < _.size)

    shortHand match {
      case head1 +: tail1 => {
        val head2 +: tail2 = longHand
        val List((winCard, winHand), (loseCard, loseHand)) = List((head1, tail1), (head2, tail2)).sortWith(_._1 > _._1)
        Game(winHand ++ Vector(winCard, loseCard), loseHand).play()
      }
      case _ => Game.computeScore(longHand)
    }
  }
}


object Day22 extends App {
  val text = io.Source.fromFile("inputs/input.txt").mkString

  val game = Game.parseText(text)
  val answer = game.play()
  println(answer)
}