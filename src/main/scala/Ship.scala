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
      println(newSlot)
      generateElements(newShip, newSlot, i + 1)
    }
  }
}
