import scala.util.Random
import Console.{BLUE, GREEN, MAGENTA, RED, RESET, UNDERLINED, YELLOW}

// case class Ship (size: Int, start: Array[String], direction: String, hit: Int)

case class History (p1: String, p1V: Int, p2: String, p2V: Int)

object game extends App {

  val cols: Int = 10
  val rows: Int = 10
  val shipSizes: Array[Int] = Array(2, 3, 3, 4, 5)
  val random1: Random = new Random
  val random2: Random = new Random
  val random3: Random = new Random

  val grid1: Grid = Grid(rows, cols, Array.ofDim[Element](rows, cols), Nil)
  val grid2: Grid = Grid(rows, cols, Array.ofDim[Element](rows, cols), Nil)


  val game = mainLoop(0, null, null, grid1, grid2, 1, History("p1", 0, "p2", 0))

  /*
  This loop is a recursive match case pattern. Each case correspond to an action of the game.
   */
  def mainLoop (i: Int, p1: Player, p2: Player, g1: Grid, g2: Grid, loop: Int, history: History): Boolean = {
    i match {
      case 0 =>
        /* Start display and player 1 informations */
        println(RED + "========== GAME START ==========" + RESET)
        println(YELLOW + "NB : All answer except player names are TO UPPURCASE.\nOtherwise you will have to give informations again.\n" + RESET)

        val newP1: Player = selectGameMode(1)
        mainLoop(1, newP1, p2, g1, g2, loop, history)

      case 1 =>
        /* Player 2 informations */
        val newP2: Player = selectGameMode(2)

        if (newP2.getIsIA() && p1.getIsIA()) {
          val newLoops = setNumberLoops()
          mainLoop(2, p1, newP2, g1, g2, newLoops, history)
        }
        else mainLoop(2, p1, newP2, g1, g2, loop, history)

      case 2 =>
        /* grids initialisation */
        val newG1 = Grid(g1.rows, g1.cols, GridController.initialize(g1.grid, 0, 0), Nil)
        val newG2 = Grid(g2.rows, g2.cols, GridController.initialize(g2.grid, 0, 0), Nil)
        println(RED + "========== PLAYER 1 TURN ==========\n" + RESET)
        mainLoop(3, p1, p2, newG1, newG2, loop, history)

      case 3 =>
        /* Player 1 ships placement */
        val newG1 = GridController.placeShips(g1, p1, p2, shipSizes, 0)
        println(RED + "========== PLAYER 2 TURN ==========\n" + RESET)
        mainLoop(4, p1, p2, newG1, g2, loop, history)

      case 4 =>
        /* Player 2 ships placement */
        val newG2 = GridController.placeShips(g2, p2, p1, shipSizes, 0)
        mainLoop(5, p1, p2, g1, newG2, loop, history)

      case 5 =>
        /* Begin game console display */
        println(RED + "Players are ready. Let's begin\n" + RESET)
        mainLoop(6, p1, p2, g1, g2, loop, history)

      case 6 =>
        /* Player 1 shoot */
        val nG2: Grid = GridController.shootOnGrid(g2, p1, p2)
        val newG2: Grid = GridController.removeEmptyShips(nG2, p1, Nil, 0)
        mainLoop(9, p1, p2, g1, newG2, loop, history)

      case 7 =>
        /* Player 2 shoot */
        val nG1: Grid = GridController.shootOnGrid(g1, p2, p1)
        val newG1: Grid = GridController.removeEmptyShips(nG1, p2, Nil, 0)
        mainLoop(8, p1, p2, newG1, g2, loop, history)

      case 8 =>
        /* We check ships of player 1 */
        if (g1.ships.isEmpty) {
          println(p2.getColor() + p2.getName() + GREEN + " has destroy all ships. He's the winner, " + RED + "congratulations.\n" + RESET)
          println(RED + "GAME END\n" + RESET)
          val newHistory: History = history.copy(p1.getName(), history.p1V, p2.getName(), history.p2V + 1)
          mainLoop(10, p1, p2, g1, g2, loop, newHistory)

        } else {
          println(RED + "========== PLAYER 1 TURN ==========\n" + RESET)
          mainLoop(6, p1, p2, g1, g2, loop, history)
        }

      case 9 =>
        /* We check ships of player 2 */
        if (g2.ships.isEmpty) {
          println(p1.getColor() + p1.getName() + GREEN + " has destroy all ships. He's the winner, " + RED + "congratulations.\n" + RESET)
          println(RED + "========== GAME END ==========\n" + RESET)
          val newHistory: History = history.copy(p1.getName(), history.p1V + 1, p2.getName(), history.p2V)
          mainLoop(10, p1, p2, g1, g2, loop, newHistory)
        } else {
          println(RED + "========== PLAYER 2 TURN ==========\n" + RESET)
          mainLoop(7, p1, p2, g1, g2, loop, history)
        }

      case 10 =>
        if (loop == 1) {
          println(YELLOW + "Do you want to restart this game ? (Y/N) > " + RESET)
          val a: String = scala.io.StdIn.readLine()
          if (a == "Y") {
            println(YELLOW + "\nGame is restarting." + RESET)
            val newHistory: History = history.copy(p2.getName(), history.p2V, p1.getName(), history.p1V)
            mainLoop(2, p2, p1, grid1, grid2, loop, newHistory)
          } else if (a == "N") {
            println(YELLOW + "\nBattleship is finished. You will find game results in a CSV file.\n" + RESET)
            println(GREEN + history.p1 + " - " + history.p1V + " V." + RESET)
            println(GREEN + history.p2 + " - " + history.p2V + " V." + RESET)
            mainLoop(11, p1, p2, g1, g2, loop, history)

          } else {
            println("\n" + RED + "Please, submit a valid answer (Y or N).\n")
            mainLoop(10, p1, p2, g1, g2, loop, history)
          }
        }
        else {
          val newHistory: History = history.copy(p2.getName(), history.p2V, p1.getName(), history.p1V)

          mainLoop(2, p2, p1, grid1, grid2, loop - 1, newHistory)
        }

      case 11 => true
    }
  }

  def selectGameMode (num: Int): Player = {
    println(YELLOW + "Select player type n°" + num + "." + RESET)
    println("0 - " + YELLOW + "Human." + RESET)
    println("1 - " + YELLOW + "IA level 1." + RESET)
    println("2 - " + YELLOW + "IA level 2." + RESET)
    println("3 - " + YELLOW + "IA level 3." + RESET)

    print(BLUE + "Planner" + GREEN + " : Submit player type > " + RESET)
    val playerType = scala.io.StdIn.readLine()
    println(playerType)
    print(BLUE + "Planner" + GREEN + " : Submit player name > " + RESET)
    val name = scala.io.StdIn.readLine()
    println(name + "\n")

    playerType match {
      case "0" =>
        Human(name, num - 1, false)
      case "1" =>
        ArtificialIntelligence1(random1, name, num - 1, true)
      case "2" =>
        new ArtificialIntelligence2(random2, name, num - 1, true)
      case "3" =>
        ArtificialIntelligence1(random3, name, num - 1, true)
      case _ =>
        println(RED + "Please, submit a valid type (0, 1, 2 or 3).\n")
        selectGameMode(num)
    }
  }

  def setNumberLoops (): Int = {
    print(BLUE + "Planner" + GREEN + " : How many games do you want to execute ? > " + RESET)
    val newLoop = scala.io.StdIn.readLine()
    try {
      val newLoopToInt = newLoop.toInt
      if (newLoopToInt < 0) {
        println(RED + "ERROR : " + newLoopToInt +" => Negative number." + RESET)
        setNumberLoops()
      }
      else newLoop.toInt
    }
    catch {
      case _: Throwable =>
        println(RED + "ERROR : " + newLoop +" => Invalid number. Try again." + RESET)
        setNumberLoops()
    }
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
