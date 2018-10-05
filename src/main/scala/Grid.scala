import scala.Console.{GREEN, GREEN_B, RED, RED_B, RESET, YELLOW}
import Helper.displayToUser

import scala.annotation.tailrec

case class Element (row: Int, col: Int, isShipHere: Boolean, isShooted: Boolean)

case class Grid (rows: Int, cols: Int, grid: Array[Array[Element]], ships: List[Ship])

object GridController {

  private val alphabet: Array[String] = Array("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")

  /*
  Return : Matrix with default element in each slot.
   */
  @tailrec
  def initialize (oldGrid: Array[Array[Element]], x: Int, y: Int): Array[Array[Element]] = {
    var newGrid = oldGrid.clone
    val maxRow = newGrid.length - 1
    val maxCol = newGrid(0).length - 1

    val element = Element(y, x, false, false)
    newGrid(y)(x) = element

    if (y == maxRow) {
      if (x == maxCol) {
        newGrid
      }
      else initialize(newGrid, x + 1, y)
    }
    else if (x == maxCol) initialize(newGrid, 0, y + 1)
    else initialize(newGrid, x + 1, y)
  }

  /*
  Return : New matrix with all ships placed inside.
   */
  @tailrec
  def placeShips (grid: Grid, player: Player, opponent: Player, shipSizes: Array[Int], i: Int): Grid = {
    if (!player.getIsAI())displayGrid(grid, player, opponent, true, -1, -1)

    var newGrid: Grid = grid.copy()
    if (i >= shipSizes.length) {
      /* All ships are placed */
      newGrid
    }
    else {
      /* We generate a ship with user inputs */
      val shipData = player.placeShip(shipSizes(i))
      val emptyShip = Ship(Nil, shipData._2, shipSizes(i), false)
      val ship = ShipController.generateElements(emptyShip, shipData._1, 0)

      val isValid: Boolean = checkShipPlacement(newGrid.grid, player, ship, 0)

      if (isValid) {
        newGrid = placeShip(newGrid, ship, 0)
        placeShips(newGrid, player, opponent, shipSizes, i + 1)
      } else {
        placeShips(newGrid, player, opponent, shipSizes, i)
      }
    }
  }

  /*
  Fix a valid ship that has been checked by "CheckShipPlacement" before.
   */
  private def placeShip (grid: Grid, ship: Ship, i: Int): Grid = {
    if (i >= ship.size) {
      val newShips: List[Ship] = ship :: grid.ships
      val newGrid = grid.copy(ships = newShips)
      newGrid
    }
    else {
      var newGrid = grid.copy()
      val xy = ship.elements(i)
      val newElement = newGrid.grid(xy.row)(xy.col).copy(isShipHere = true)
      newGrid.grid(xy.row)(xy.col) = newElement
      placeShip(newGrid, ship, i + 1)
    }
  }

  /*
  Check if the generated elements fits with the grid and if there is no ship on the newShip placement.
   */
  @tailrec
  private def checkShipPlacement (grid: Array[Array[Element]], player: Player, ship: Ship, i: Int): Boolean = {
    if (i == ship.elements.size) true
    else {
      val x = ship.elements(i).col
      val y = ship.elements(i).row
      /* We check if all ship parts are inside the grid */
      if (x < 0 || x >= grid(0).length || y < 0 || y >= grid.length) {
        displayToUser(Some(player), None, "Sorry, this ship is outside of the grid. Try again.", 4, true)
        false

      } else {
        if (grid(y)(x).isShipHere) {
          displayToUser(Some(player), None, "Sorry, there is already a ship on these locations. Try again.", 4, true)
          false
        }
        else checkShipPlacement(grid, player, ship, i + 1)
      }
    }
  }

  /*
  Display the grid with labels. If showShip : It's the current user grid. Else it's the opponent grid.
   */
  @tailrec
  def displayGrid (grid: Grid, player: Player, opponent: Player, showShip: Boolean, x: Int, y: Int): Unit = {
    var newX: Int = x
    var newY: Int = y
    if (!(x == -1 && y == grid.cols)) {
      y match {
        /* We are on the first row labels */
        case -1 =>
          if (x == -1) {
            if (showShip) {
              displayToUser(Some(player), Some(opponent), "ship grid", 2, false)
            } else {
              displayToUser(Some(player), Some(opponent), "please shoot on a new slot of " + opponent.getColor() + opponent.getName() + YELLOW +  "'s grid.", 2, false)
            }
            print(YELLOW + "\\ " + RESET)
            newX += 1
          } else if (x < grid.cols) {
            /* We set the right letter label */
            print(YELLOW + alphabet(x) + " " + RESET)
            newX += 1
          } else {
            /* We go to next row */
            println("")
            newX = -1
            newY += 1
          }
        case _ =>
          /* We are on first col of each rows */
          if (x == -1) {
            /* We set the right number label */
            print(YELLOW + y + " " + RESET)
            newX += 1
          } else if (x < grid.cols) {
            /* We check if there is a ship */
            if (grid.grid(y)(x).isShipHere) {
              if (grid.grid(y)(x).isShooted) print(YELLOW + RED_B + "X " + RESET)
              else {
                if (showShip) print(GREEN + "X " + RESET)
                else print(". ")
              }
            }
            else {
              if (grid.grid(y)(x).isShooted) print(GREEN_B + ". " + RESET)
              else print(". ")
            }
            newX += 1
          } else {
            /* We go to next row */
            println("")
            newX = -1
            newY += 1
          }
      }
      displayGrid(grid, player, opponent, showShip, newX, newY)
    } else {
      println("")
    }
  }

  /*
  Return : Updated grid with the shoot of the user.
   */
  def shootOnGrid (grid: Grid, player: Player, opponent: Player): Grid = {
    if (!player.getIsAI()) displayGrid(grid, player, opponent, false, -1, -1)
    displayToUser(Some(player), None, "where do you want to shoot ?", 2, false)
    val xy: Array[Int] = player.play()

    /* Check if the shot is on the grid. */
    if (checkShoot(grid, xy, player)) {
      val x = xy(1)
      val y = xy(0)

      /* We update the grid */
      val newElement: Element = grid.grid(y)(x).copy(isShooted = true)
      val newShips: List[Ship] = shootShip(player, grid.ships, Nil, x, y, 0)
      var newG: Array[Array[Element]] = grid.grid.clone()
      newG(y)(x) = newElement
      val newGrid: Grid = grid.copy(grid = newG, ships = newShips)
      if (!player.getIsAI() || !opponent.getIsAI()) displayGrid(newGrid, player, opponent, true, -1, -1)
      else if (!player.getIsAI() && !opponent.getIsAI()) displayGrid(newGrid, player, opponent, false, -1, -1)

      /* We display the result */
      if (grid.grid(y)(x).isShipHere) {
        displayToUser(Some(player), Some(opponent), "has shot on target, nice.", 3, true)
        player.isShipTouched = 1
        player.targetLocked = true
      } else {
        displayToUser(Some(player), Some(opponent), "has shot in the water, looser.", 3, true)
        player.isShipTouched = 2
      }

      newGrid
      // end
    } else {
      player.isShipTouched = 2
      shootOnGrid(grid, player, opponent)
    }
  }

  /*
  Return : Ship list updated according to the xy shoot. 1 ship will be updated.
   */
  def shootShip (player: Player, ships: List[Ship], updatedShips: List[Ship], x: Int, y: Int, i: Int): List[Ship] = {
    if (i >= ships.size) updatedShips
    else {
      /* We check each ship and add them to the new ship list */
      val newShip: Ship = ShipController.isTouched(player, ships(i), Nil,  x , y, 0)
      shootShip(player, ships, newShip :: updatedShips, x, y, i + 1)
    }
  }

  /*
  Return : True if the shoot is on the grid. False if not.
   */
  private def checkShoot (grid: Grid, shoot: Array[Int], player: Player): Boolean = {
    val x = shoot(1)
    val y = shoot(0)
    if (x < 0 || x >= grid.grid(0).length || y < 0 || y >= grid.grid.length) {
      displayToUser(Some(player), None, "ERROR : This shoot is outside of the grid. Try again.", 4, true)
      false
    } else if (grid.grid(y)(x).isShooted && player.getLvlAI > 1) {
      // displayToUser(Some(player), None, "ERROR : You have already shoot here. Try again.", 4, true)
      false
    } else {
      true
    }
  }

  /*
  We remove every ships that are dead.
   */
  def removeEmptyShips (grid: Grid, player: Player, opponent: Player, updatedShips: List[Ship], i: Int): Grid = {
    if (grid.ships.size == i) {
      val newGrid: Grid = grid.copy(ships = updatedShips)
      newGrid
    } else {
      /* If the ship has no elements, we remove it from the ships list of the grid */
      if (grid.ships(i).isDead) {
        player.isShipTouched = 0
        player.targetLocked = false
        ShipController.killShipMessage(player, opponent, grid.ships(i).size)
        removeEmptyShips(grid, player, opponent, updatedShips, i + 1)
      }
      else removeEmptyShips(grid, player, opponent, grid.ships(i) :: updatedShips, i + 1)
    }
  }
}
