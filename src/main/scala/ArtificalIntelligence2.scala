import scala.util.Random
import game.cols
import game.rows

class ArtificialIntelligence2 (override val random: Random, override val name: String, override val color: Int, override val isIA: Boolean) extends ArtificialIntelligence1(random, name, color, isIA) {

  override def play (): Array[Int] = {
    val x = random.nextInt(cols)
    val y = random.nextInt(rows)
    println("Ok AI proposed : " + y + " and " + x)
    Array(y, x)
  }
}

/*class ArtificialIntelligence2(random: Random, name: String, color: Int) extends ArtificialIntelligence1(random, name, color) {
  val lastSlotHitted: Array[Int] = Array(-1, -1)
  val nbSlotHitted: Int = 0
  val shipOrientation: String = "right"

  override def play (grid: Array[Array[Boolean]]): Array[Array[Boolean]] = {

    /* We look if the last shot was on a boat or not */
    var slot: Array[Int] = this.think(grid, nbSlotHitted, shipOrientation, lastSlotHitted)

    slot = this.chooseASlot(grid, random, Array(-1, -1), Array(-1, -1))
    var newGrid: Array[Array[Boolean]] = grid.clone
    if (!(slot sameElements Array(-1, -1))) newGrid(slot(0))(slot(1)) = true
    newGrid
  }

  def think (grid: Array[Array[Boolean]], nbSlotH: Int, shipO: String, lastSlotH: Array[Int]): Array[Int] = {
    var x: Int = lastSlotH(0)
    var y: Int = lastSlotH(1)
    var newSipO: String = shipO
    if (nbSlotHitted > 0) {
      shipOrientation match {
        case "right" => x += 1
        case "left" => x -= 1
        case "top" => y -= 1
        case "bottom" => y += 1
        case "_" =>
          x = -1
          y = -1
      }
      x match {
        case grid.length || -2 =>
          shipOrientation match {
            case "right" =>
              newSipO = "left"
              x -= nbSlotHitted - 1
            case "left" =>
              newSipO = "right"
              x += nbSlotHitted + 2
            case "bottom" =>
              newSipO = "top"
              y -= nbSlotHitted - 1
            case "top" =>
              newSipO = "bottom"
              y += nbSlotHitted + 2
          }
      }
    }
    Array(x, y)
  }
}*/
