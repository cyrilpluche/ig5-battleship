import Helper.displayToUser

import scala.annotation.tailrec

case class Ship (elements: List[Element], orientation: String, size: Int, isDead: Boolean)

object ShipController {

  def generateElements (ship: Ship, slot: Array[Int], i: Int): Ship = {
    if (i > ship.size - 1) ship
    else {
      /* We create the element. Add it to the list. Generate the new Ship */
      val element = Element(slot(0), slot(1), true, false)
      val newElements: List[Element] = element :: ship.elements
      val newShip = Ship(newElements, ship.orientation, ship.size, ship.isDead)

      var newSlot = slot.clone()
      ship.orientation match {
        case "right" => newSlot(1) += 1
        case "left" => newSlot(1) -= 1
        case "top" => newSlot(0) -= 1
        case "bottom" => newSlot(0) += 1
      }
      generateElements(newShip, newSlot, i + 1)
    }
  }

  @tailrec
  def isTouched (player: Player, ship: Ship, updatedElement: List[Element], x: Int, y: Int, i: Int): Ship = {
    if (i >= ship.elements.length) {
      if (updatedElement.isEmpty) {
        ship.copy(elements = Nil, isDead = true)
      }
      else ship.copy(elements = updatedElement)
    }
    else {
      if (ship.elements(i).row == y && ship.elements(i).col == x) {
        isTouched(player, ship, updatedElement, x, y, i + 1)
      }
      else isTouched(player, ship, ship.elements(i) :: updatedElement, x, y, i + 1)
    }
  }

  def killShipMessage (player: Player, opponent: Player, s: Int): Unit = {
    displayToUser(Some(player), Some(opponent), "has destroy a ship of size" + s + ". The other player said " + Console.RED + "\'You sank my battleship!\'.", 3, true)
  }
}
