import scala.util.Random
import Game.cols
import Game.rows

class AI3(override val random: Random, override val name: String, override val color: Int, override val isIA: Boolean, lvlAI: Int) extends AI2(random, name, color, isIA, lvlAI) {

  override def play(): Array[Int] = {
    isShipTouched match {
      case 0 =>
        /* We are searching for random shot */
        val x = random.nextInt(cols)
        val y = random.nextInt(rows)
        orientationChosen = orientations(0)
        nbOrientationTried = 0
        lastShot = Array(y, x)
        Array(y, x)

      case 1 =>
        /* We finally found a ship or we continue to shoot a ship */
        orientationChosen match {
          case "right" => lastShot(1) += 1
          case "left" => lastShot(1) -= 1
          case "bottom" => lastShot(0) += 1
          case "top" => lastShot(0) -= 1
        }
        lastShot

      case 2 =>
        /* We missed the ship */
        if (!targetLocked) {
          orientationChosen = orientations(0)
          isShipTouched = 0
        }
        else if (nbOrientationTried >= 3) {
          isShipTouched = 0
        } else {
          // We come back to previous case
          orientationChosen match {
            case "right" => lastShot(1) -= 1
            case "left" => lastShot(1) += 1
            case "bottom" => lastShot(0) -= 1
            case "top" => lastShot(0) += 1
          }
          nbOrientationTried += 1
          orientationChosen = orientations(nbOrientationTried)
          isShipTouched = 1
        }
        play()
    }
  }

}