case class Element (rowLabel: String, colLabel: String, isShipHere: Boolean, isShooted: Boolean)

case class Ship (elements: List[Element], orientation: String, size: Int, isDead: Boolean)

case class Grid (rows: Int, cols: Int, grid: Array[Array[Element]])

object GridController {

  /*
  Return : Matrix with default element in each slot.
   */
  def initialize (oldGrid: Array[Array[Element]], x: Int, y: Int): Array[Array[Element]] = {
    var newGrid = oldGrid.clone
    val maxRow = newGrid.length - 1
    val maxCol = newGrid(0).length - 1

    var element = Element("1", "A", false, false)
    newGrid(x)(y) = element

    if (x == maxRow) {
      if (y == maxCol) {
        newGrid
      }
      else initialize(newGrid, x, y + 1)
    }
    else if (y == maxCol) initialize(newGrid, x + 1, 0)
    else initialize(newGrid, x, y + 1)
  }

  /*
  Return : New matrix with all ships placed inside.
   */
  def placeShips (grid: Grid, player: Player, shipSizes: Array[Int], i: Int): Grid = {
    val newGrid: Grid = grid.copy()
    i match {
      /* All ships are placed */
      case shipSizes.length => newGrid
      case _ =>
        val shipData = player.placeShip(shipSizes(i))
        val ship = Ship()
        val isValid: Boolean = checkShipPlacement(newGrid.grid, ship, shipSizes(i))

        if (isValid) {
          newGrid = placeShip(newGrid, ship)
          placeShips(newGrid, player, shipSizes, i + 1)
        } else {
          placeShips(newGrid, player, shipSizes, i)
        }

    }
  }

  private def placeShip (grid: Array[Array[Element]], ship: Any): Array[Array[Element]] = {

  }

  private def checkShipPlacement (grid: Array[Array[Element]], ship: Any, size: Int): Boolean = {

  }


}
