import scala.util.Random

class ArtificialIntelligence1(random: Random, name: String, color: Int) extends Player(name, color) {

  def play (grid: Array[Array[Boolean]]): Array[Array[Boolean]] = {
    var slot: Array[Int] = this.chooseASlot(grid, random, Array(-1, -1), Array(-1, -1))
    var newGrid: Array[Array[Boolean]] = grid.clone
    if (!(slot sameElements Array(-1, -1))) newGrid(slot(0))(slot(1)) = true
    newGrid
  }

  /*
  Choose a slot of the grid that hasn't been visited.
  Return : [-1, -1] if all slots had been visited. Else [x, y] a location of a grid slot.
   */
  def chooseASlot (grid: Array[Array[Boolean]], r: Random, s: Array[Int], fs: Array[Int]) : Array[Int] = {
    var limitX: Int = grid.length - 1
    var limitY: Int = grid(0).length - 1
    var slot: Array[Int] = s

    if (slot sameElements Array(-1, -1)) {
      /* If it's the first loop of the recursion, we start by picking a slot location */
      slot = Array(r.nextInt(limitX), r.nextInt(limitY))
    }

    if (!grid(slot(0))(slot(1))) {
      /* This slot hasn't been shot, we can selected it */
      return slot
    } else if (slot(1) <= limitY) {
      /* We point the next slot of the row */
      slot(1) += 1
    } else if (slot(0) <= limitX) {
      /* We point the first slot of the next row */
      slot(0) += 1
      slot(1) = 0
    } else {
      /* We start back from the first slot */
      slot(0) = 0
      slot(1) = 0
    }

    if (slot sameElements fs) {
      /* We have run over all grid slots */
      return Array(-1, -1)
    }


    /* We test next slot */
    chooseASlot(grid, r, slot, fs)
  }
}
