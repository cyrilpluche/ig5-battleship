import scala.util.Random
import game.cols
import game.rows

case class ArtificialIntelligence1 (random: Random, name: String, color: Int, isIA: Boolean) extends Player(name, color, isIA) {

  val orientationLabels: Array[String] = Array("right", "left", "top", "bottom")

  def play (): Array[Int] = {
    val x = random.nextInt(cols)
    val y = random.nextInt(rows)
    println("Ok AI proposed : " + y + " and " + x)

    Array(y, x)
  }

  def placeShip (size: Int): (Array[Int], String) = {
    val origin: Array[Int] = play()
    val randomOrientation: Int = random.nextInt(4)
    val orientation = orientationLabels(randomOrientation)
    (origin, orientation)
  }
}
