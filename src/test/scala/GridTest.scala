import org.scalatest.FunSuite

import scala.util.Random

class GridTest extends FunSuite {

  val g: Grid = Grid(10, 10, Array.ofDim[Element](10, 10), Nil)
  val g1 = GridController.initialize(g.grid, 0, 0)
  val p1 = new AI3(new Random, "AI3", 0, true, 3)
  val p2 = new AI2(new Random, "AI2", 1, true, 2)


  val test1: Boolean = initializeTest(g, 0, 0)
  val test2: Boolean = placeShipsTest(g, p1, p2, Array(2, 3, 3, 4, 5), 0, 0, 0)

  /* All slots are initialized with an Element class. We check that each row and column are well initialized. */
  def initializeTest (grid: Grid, x: Int, y: Int): Boolean = {
    if (x == 0 && y == 10) true
    else {
      val gInit = GridController.initialize(g.grid, 0, 0)
      val slot = gInit(y)(x)
      test("initializeTest/" + y.toString() + "/" + x.toString()) {
        assert(slot.row == y && slot.col == x && !slot.isShipHere && !slot.isShooted)
      }
      if (x == 9) initializeTest(grid, 0, y + 1)
      else initializeTest(grid, x + 1, y)
    }
  }

  /* After the player place his ships, we check if there is 17 slot with a ship (2 + 3 + 3 + 4 + 5) */
  def placeShipsTest (grid: Grid, player: Player, opponent: Player, shipSizes: Array[Int], x: Int, y: Int, nbShip: Int): Boolean = {
    if (x == 0 && y == 10) {
      test("placeShipsTest") {
        assert(nbShip == 17)
      }
      true
    } else {
      var newNbShip = nbShip
      if (x == 0 && y == 0) {
        val newG1 = GridController.placeShips(grid, p1, p2, shipSizes, 0)
        if (newG1.grid(y)(x).isShipHere) newNbShip += 1
        placeShipsTest(newG1, player, opponent, shipSizes, x + 1, y, newNbShip)
      } else {
        if (grid.grid(y)(x).isShipHere) newNbShip += 1
        if (x == 9) placeShipsTest(grid, player, opponent, shipSizes, 0, y + 1, newNbShip)
        else placeShipsTest(grid, player, opponent, shipSizes, x + 1, y, newNbShip)
      }

    }

  }
}
