import scala.util.Random

// case class Ship (size: Int, start: Array[String], direction: String, hit: Int)

object game extends App {

  val cols: Int = 10
  val rows: Int = 10
  val shipSizes: Array[Int] = Array(5, 4, 3, 3, 2)

  val g: Grid = Grid(rows, cols, Array.ofDim[Element](rows, cols))

  val game = mainLoop(g)

  def mainLoop(grid: Grid): Boolean = {
    println("GAME START")

    /* We initialize grids */
    val g1 = Grid(grid.rows, grid.cols, GridController.initialize(grid.grid, 0, 0))
    val g2 = g1.copy()

    val p1 = Player("Cyril", 0)
    val p2 = Player("Enzo", 1)

    val g11 = GridController.placeShips(grid, p1, shipSizes, 0)
    //val g22 = GridController.placeShips(grid, p2, shipSizes, 0)

    println(g11.grid)
    //println(g22)

    true
  }

  /*val ia1: Player = new ArtificialIntelligence1(new Random, "IA1")
  val ia2: Player = new ArtificialIntelligence2(new Random, "IA2")

  val row: Array[Boolean] = Array(false, false, false, false, false , false)
  val grid1: Array[Array[Boolean]] = Array(row.clone, row.clone, row.clone, row.clone, row.clone, row.clone)
  val grid2: Array[Array[Boolean]] = grid1.clone

  val s1 = Array(
    Ship(1, new Array[String](2), "", 0),
    Ship(2, new Array[String](2), "", 0),
    Ship(2, new Array[String](2), "", 0)
  )
  val s2 = Array(
    Ship(1, new Array[String](2), "", 0),
    Ship(2, new Array[String](2), "", 0),
    Ship(2, new Array[String](2), "", 0)
  )

  // val p1 = Player("Cyril", s1, 0)
  // val p2 = Player("Mario", s2, 0)

  val r = MainLoop(ia1, ia2, grid1, grid2)

  def MainLoop (player1: Player, player2: Player, grid1: Array[Array[Boolean]], grid2: Array[Array[Boolean]]): Unit = {
    var newGrid: Array[Array[Boolean]] = null
    for (i <- 1 to 34) {
      newGrid = player1.play(grid1)
    }

    this.printGrid(newGrid)

  }

  def printGrid (grid: Array[Array[Boolean]]): Unit = {
    val limitX: Int = grid.length-1
    val limitY: Int = grid(0).length-1

    for (x <- 0 to limitX) {
      for (y <- 0 to limitY) {
        if (grid(x)(y)) print(" 0 -")
        else print(" 1 -")
      }
      println(" ")
    }
  }*/
}
