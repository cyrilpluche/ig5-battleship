case class Element (row: Int, col: Int, isShipHere: Boolean, isShooted: Boolean)

case class Grid (rows: Int, cols: Int, grid: Array[Array[Element]])

object GridController {

  /*
  Return : Matrix with default element in each slot.
   */
  def initialize (oldGrid: Array[Array[Element]], x: Int, y: Int): Array[Array[Element]] = {
    var newGrid = oldGrid.clone
    val maxRow = newGrid.length - 1
    val maxCol = newGrid(0).length - 1

    var element = Element(x, y, false, false)
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
    var newGrid: Grid = grid.copy()
    val length: Int = shipSizes.length
    i match {
      /* All ships are placed */
      case 5 => newGrid
      case _ =>
        val shipData = player.placeShip(shipSizes(i))
        val emptyShip = Ship(Nil, shipData._2, shipSizes(i), false)
        val ship = ShipController.generateElements(emptyShip, shipData._1, 0)
        println("Yes ! Data ok")
        val isValid: Boolean = checkShipPlacement(newGrid.grid, ship)
        println("Yes ! Placement ok")


        if (isValid) {
          newGrid = placeShip(newGrid, ship, 0)
          println("yes ship placed")
          placeShips(newGrid, player, shipSizes, i + 1)
        } else {
          placeShips(newGrid, player, shipSizes, i)
        }

    }
  }

  private def placeShip (grid: Grid, ship: Ship, i: Int): Grid = {
    println("i : " + i + " - ship.size : " + ship.size)
    if (i >= ship.size) grid
    else {
      var newGrid = grid.copy()
      val xy = ship.elements(i)
      println(ship.elements)
      val newElement = newGrid.grid(xy.row)(xy.col).copy(isShipHere = true)
      newGrid.grid(xy.row)(xy.col) = newElement
      placeShip(newGrid, ship, i + 1)
    }
  }

  private def checkShipPlacement (grid: Array[Array[Element]], ship: Ship): Boolean = {
    true
  }


}
