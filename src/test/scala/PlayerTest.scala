import org.scalatest.FunSuite

import scala.annotation.tailrec

class PlayerTest extends FunSuite {

  val colsTest: Array[String] = Array("A", "B", "B", "d", "$", "&", "<")
  val rowsTest: Array[Int] = Array(1, 14, -1, 5, 4, 2, 6)
  val orientationsTest: Array[String] = Array("right", "left", "top", "bottom", "droite", "gauche", "40")

  val human = Human("PlayerTest", 0)

  val test1 = checkPlayInputsTest(human, colsTest, rowsTest, 0)
  val test2 = checkPlaceShipInputs(human, orientationsTest, 0)

  /* Only uppercase Cols with 0 < Rows < 10 are accepted. Only the first couple i = 0 is true */
  def checkPlayInputsTest (player: Player, cols: Array[String], rows: Array[Int], i: Int): Boolean = {
    if (i == cols.size) true
    else {
      test(player.getName() + ".checkPlayInputs/" + i.toString()) {
        val isValid = player.checkPlayInputs(cols(i), rows(i))
        if (i < 2) assert(isValid)
        else assert(!isValid)
      }
      checkPlayInputsTest(player, cols, rows, i + 1)
    }
  }

  def checkPlaceShipInputs (player: Player, orientations: Array[String], i: Int): Boolean = {
    if (i == orientations.size) true
    else {
      test(player.getName() + ".checkPlaceShip/" + i.toString()) {
        val isValid = player.checkPlaceShipInputs(orientations(i))
        if (i < 4) assert(isValid)
        else assert(!isValid)
      }
      checkPlaceShipInputs(player, orientations, i + 1)
    }
  }
}
