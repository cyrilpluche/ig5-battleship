import scala.util.Random
import Game.cols
import Game.rows

case class AI1(random: Random, name: String, color: Int, isIA: Boolean, lvlAI: Int) extends Player(name, color, isIA, lvlAI) {

  val orientationLabels: Array[String] = Array("right", "left", "top", "bottom")

  def play (): Array[Int] = {
    val x = random.nextInt(cols)
    val y = random.nextInt(rows)
    Array(y, x)
  }

  def placeShip (size: Int): (Array[Int], String) = {
    val origin: Array[Int] = play()
    val randomOrientation: Int = random.nextInt(4)
    val orientation = orientationLabels(randomOrientation)
    (origin, orientation)
  }
}
