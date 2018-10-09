import org.scalatest.FunSuite

class ShipTest extends FunSuite {

  val s = Ship(Nil, "right", 5, false)
  val test1: Boolean = generateElementsTest(s, Array(3, 4), s.size - 1)

  /* We check that all part of the ship have the right col and row inside his elements.
   * We also need to execute the recursive call from the end to 0 (ship parts are inverted inside ship elements) */
  def generateElementsTest (ship: Ship, slot: Array[Int], i: Int): Boolean = {
    if (i == -1) true
    else {
      if (i == s.size - 1) {
        val newShip = ShipController.generateElements(ship, slot, 0)
        test("generateElementsTest/" + i.toString()) {
          assert(newShip.elements(i).row == slot(0) && newShip.elements(i).col == slot(1))
        }
        val newSlot = Array(slot(0), slot(1) + 1)
        generateElementsTest(newShip, newSlot, i - 1)
      } else {
        test("generateElementsTest/" + i.toString()) {
          assert(ship.elements(i).row == slot(0) && ship.elements(i).col == slot(1))
        }
        val newSlot = Array(slot(0), slot(1) + 1)
        generateElementsTest(ship, newSlot, i - 1)
      }
    }
  }

}
