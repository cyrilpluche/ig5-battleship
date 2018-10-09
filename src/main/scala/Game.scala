// import au.com.bytecode.opencsv.CSVWriter
import java.io.{BufferedWriter, FileWriter}
import scala.util.Random
import Console.{BLUE, GREEN, MAGENTA, RED, RESET, UNDERLINED, YELLOW}
import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import scala.util.Random
import au.com.bytecode.opencsv.CSVWriter

// case class Ship (size: Int, start: Array[String], direction: String, hit: Int)

case class History (p1: String, p1V: Int, p2: String, p2V: Int)

object Game extends App {

  val cols: Int = 10
  val rows: Int = 10
  val shipSizes: Array[Int] = Array(2, 3, 3, 4, 5)
  val random1: Random = new Random
  val random2: Random = new Random
  val random3: Random = new Random
  val nbResultDisplayed = 1

  val grid1: Grid = Grid(rows, cols, Array.ofDim[Element](rows, cols), Nil)
  val grid2: Grid = Grid(rows, cols, Array.ofDim[Element](rows, cols), Nil)


  val game = mainLoop(0, null, null, grid1, grid2, 1, Array(History("AI1", 0, "AI2", 0), History("AI1", 0, "AI3", 0), History("AI2", 0, "AI3", 0)), 0)

  /*
  This loop is a recursive match case pattern. Each case correspond to an action of the game.
   */
  @tailrec
  def mainLoop (i: Int, p1: Player, p2: Player, g1: Grid, g2: Grid, loop: Int, history: Array[History], isTestAI: Int): Boolean = {
    i match {
      case 0 =>
        /* Start display and player 1 informations */
        println(RED + "========== GAME START ==========" + RESET)

        /*if (isTestAI == 0) {*/
          println(YELLOW + "NB : All answer except player names are TO UPPURCASE.\nOtherwise you will have to give informations again.\n" + RESET)
          /* Initial case : choose between casual game or AI test */
          val isTestselected: Boolean = activeTest()
          if (!isTestselected) {
            /* Casual game */
            val newP1: Player = selectGameMode(1)
            mainLoop(1, newP1, p2, g1, g2, loop, history, isTestAI)
          } else {
            /* AI test */
            val players: Array[Player] = selectTest(3)
            mainLoop(2, players(0), players(1), g1, g2, 100, history, 3)
          }

      case 1 =>
        /* Player 2 informations */
        val newP2: Player = selectGameMode(2)

        if (newP2.getIsAI() && p1.getIsAI()) {
          val newLoops = setNumberLoops()
          mainLoop(2, p1, newP2, g1, g2, newLoops, history, isTestAI)
        }
        else mainLoop(2, p1, newP2, g1, g2, loop, history, isTestAI)

      case 2 =>
        /* grids initialisation */
        val newG1 = Grid(g1.rows, g1.cols, GridController.initialize(g1.grid, 0, 0), Nil)
        val newG2 = Grid(g2.rows, g2.cols, GridController.initialize(g2.grid, 0, 0), Nil)
        if (!p1.getIsAI() || !p2.getIsAI()) println(RED + "========== PLAYER 1 TURN ==========\n" + RESET)
        mainLoop(3, p1, p2, newG1, newG2, loop, history, isTestAI)

      case 3 =>
        /* Player 1 ships placement */

        val newG1 = GridController.placeShips(g1, p1, p2, shipSizes, 0)
        if (!p2.getIsAI() || !p1.getIsAI()) println(RED + "========== PLAYER 2 TURN ==========\n" + RESET)
        mainLoop(4, p1, p2, newG1, g2, loop, history, isTestAI)

      case 4 =>
        /* Player 2 ships placement */
        val newG2 = GridController.placeShips(g2, p2, p1, shipSizes, 0)
        mainLoop(5, p1, p2, g1, newG2, loop, history, isTestAI)

      case 5 =>
        /* Begin game console display */
        println(RED + "Players are ready. Let's begin\n" + RESET)
        mainLoop(6, p1, p2, g1, g2, loop, history, isTestAI)

      case 6 =>
        /* Player 1 shoot */
        val nG2: Grid = GridController.shootOnGrid(g2, p1, p2)
        val newG2: Grid = GridController.removeEmptyShips(nG2, p1, p2, Nil, 0)
        mainLoop(9, p1, p2, g1, newG2, loop, history, isTestAI)

      case 7 =>
        /* Player 2 shoot */
        val nG1: Grid = GridController.shootOnGrid(g1, p2, p1)
        val newG1: Grid = GridController.removeEmptyShips(nG1, p2, p1, Nil, 0)
        mainLoop(8, p1, p2, newG1, g2, loop, history, isTestAI)

      case 8 =>
        /* We check ships of player 1 */
        if (g1.ships.isEmpty) {

          if (loop <= nbResultDisplayed) {
            GridController.displayGrid(g1, p1, p2, 0, -1, -1)

            GridController.displayGrid(g2, p2, p1, 0, -1, -1)
          }

          println(p2.getColor() + p2.getName() + GREEN + " has destroy all ships. He's the winner, " + RED + "congratulations.\n" + RESET)
          println(RED + "========== GAME END ==========\n" + RESET)

          if (isTestAI == 0) {
            val newHistory: History = history(0).copy(p1.getName(), history(0).p1V, p2.getName(), history(0).p2V + 1)
            mainLoop(10, p1, p2, g1, g2, loop, Array(newHistory), isTestAI)
          } else {
            val newHistory = updateAIHistory(p2.getName(), history, isTestAI)
            mainLoop(10, p1, p2, g1, g2, loop, newHistory, isTestAI)
          }

        } else {
          if (!p1.getIsAI() || !p2.getIsAI()) println(RED + "========== PLAYER 1 TURN ==========\n" + RESET)
          mainLoop(6, p1, p2, g1, g2, loop, history, isTestAI)
        }

      case 9 =>
        /* We check ships of player 2 */
        if (g2.ships.isEmpty) {

          if (loop <= nbResultDisplayed) {
            GridController.displayGrid(g2, p2, p1, 0, -1, -1)
            GridController.displayGrid(g1, p1, p2, 0, -1, -1)
          }
          println(p1.getColor() + p1.getName() + GREEN + " has destroy all ships. He's the winner, " + RED + "congratulations.\n" + RESET)
          println(RED + "========== GAME END ==========\n" + RESET)

          if (isTestAI == 0) {
            val newHistory: History = history(0).copy(p1.getName(), history(0).p1V + 1, p2.getName(), history(0).p2V)
            mainLoop(10, p1, p2, g1, g2, loop, Array(newHistory), isTestAI)
          } else {
            val newHistory = updateAIHistory(p1.getName(), history, isTestAI)
            mainLoop(10, p1, p2, g1, g2, loop, newHistory, isTestAI)
          }

        } else {
          if (!p2.getIsAI() || !p1.getIsAI()) println(RED + "========== PLAYER 2 TURN ==========\n" + RESET)
          mainLoop(7, p1, p2, g1, g2, loop, history, isTestAI)
        }

      case 10 =>
        p1.isShipTouched = 0
        p1.targetLocked = false
        p2.isShipTouched = 0
        p2.targetLocked = false

        if (isTestAI > 1) {
          if (loop == 1) {
            /* We have execute all games of one versus */
            val players: Array[Player] = selectTest(isTestAI - 1)
            mainLoop(2, players(0), players(1), g1, g2, 100, history, isTestAI - 1)
          } else {
            /* We just invert */
            mainLoop(2, p2, p1, grid1, grid2, loop - 1, history, isTestAI)
          }
        }
        else if (isTestAI == 1 && loop == 1) {
          println(YELLOW + "\nBattleship is finished. You will find game results in a CSV file.\n" + RESET)

          exportCSV(history)

          print(GREEN + history(0).p1 + " - " + history(0).p1V + " V. || " + RESET)
          println(GREEN + history(0).p2 + " - " + history(0).p2V + " V." + RESET)

          print(GREEN + history(1).p1 + " - " + history(1).p1V + " V. || " + RESET)
          println(GREEN + history(1).p2 + " - " + history(1).p2V + " V." + RESET)

          print(GREEN + history(2).p1 + " - " + history(2).p1V + " V. || " + RESET)
          println(GREEN + history(2).p2 + " - " + history(2).p2V + " V." + RESET)

          mainLoop(11, p1, p2, g1, g2, loop, history, isTestAI)
        }
        else if (isTestAI == 1) {
          /* we invert for AI Test */
          mainLoop(2, p2, p1, grid1, grid2, loop - 1, history, isTestAI)
        }
        else if (loop == 1) {
          /* End of casual loop */
          println(GREEN + history(0).p1 + " - " + history(0).p1V + " V." + RESET)
          println(GREEN + history(0).p2 + " - " + history(0).p2V + " V.\n" + RESET)
          print(YELLOW + "Do you want to play more ? (Y/N) > " + RESET)
          val a: String = scala.io.StdIn.readLine()
          println(a)

          if (a == "Y") {
            println(YELLOW + "\nGame is restarting.\n" + RESET)
            val newHistory: History = history(0).copy(p2.getName(), history(0).p2V, p1.getName(), history(0).p1V)
            val newLoop: Int = setNumberLoops()
            mainLoop(2, p2, p1, grid1, grid2, newLoop, Array(newHistory), isTestAI)
          } else if (a == "N") {
            println(YELLOW + "\nBattleship is finished.\n" + RESET)
            mainLoop(11, p1, p2, g1, g2, loop, history, isTestAI)

          } else {
            println("\n" + RED + "Please, submit a valid answer (Y or N).\n")
            mainLoop(10, p1, p2, g1, g2, loop, history, isTestAI)
          }
        }
        else {
          /* New loop for casual games */
          val newHistory: History = history(0).copy(p2.getName(), history(0).p2V, p1.getName(), history(0).p1V)
          println(RED + "========== GAME START ========== \n" + RESET)
          mainLoop(2, p2, p1, grid1, grid2, loop - 1, Array(newHistory), isTestAI)
        }

      case 11 =>
        true
    }
  }

  def selectGameMode (num: Int): Player = {
    println(YELLOW + "Select player type n°" + num + "." + RESET)
    println("0 - " + YELLOW + "Human." + RESET)
    println("1 - " + YELLOW + "AI level 1." + RESET)
    println("2 - " + YELLOW + "AI level 2." + RESET)
    println("3 - " + YELLOW + "AI level 3." + RESET)

    print(BLUE + "Planner" + GREEN + " : Submit player type > " + RESET)
    val playerType = scala.io.StdIn.readLine()
    println(playerType)

    playerType match {
      case "0" =>
        print(BLUE + "Planner" + GREEN + " : Submit player name > " + RESET)
        val name = scala.io.StdIn.readLine()
        println(name + "\n")
        Human(name, num - 1)
      case "1" =>
        AI1(random1, "AI1", num - 1, true, 1)
      case "2" =>
        new AI2(random2, "AI2", num - 1, true, 2)
      case "3" =>
        new AI3(random3, "AI3", num - 1, true, 3)
      case _ =>
        println(RED + "Please, submit a valid type (0, 1, 2 or 3).\n")
        selectGameMode(num)
    }

  }

  def activeTest (): Boolean = {
    println(YELLOW + "Do you want to execute the AI tests ? (Y/N) > " + RESET)
    val a: String = scala.io.StdIn.readLine()
    println(a)
    if (a == "Y") {
      println(YELLOW + "\nAI tests activated.\n" + RESET)
      true
    } else if (a == "N") {
      false
    } else {
      println("\n" + RED + "Please, submit a valid answer (Y or N).\n")
      activeTest()
    }
  }

  def selectTest (indiceTest: Int): Array[Player] = {
    indiceTest match {
      case 3 =>
        /* 1 vs 2 */
        val p1 = AI1(random1, "AI1", 0, true, 1)
        val p2 = new AI2(random2, "AI2", 1, true, 2)
        Array(p1, p2)

      case 2 =>
      /* 1 vs 3 */
        val p1 = AI1(random1, "AI1", 0, true, 1)
        val p3 = new AI3(random2, "AI3", 1, true, 3)
        Array(p1, p3)
      case 1 =>
      /* 2 vs 3 */
        val p2 = new AI2(random2, "AI2", 1, true, 2)
        val p3 = new AI3(random2, "AI3", 1, true, 3)
        Array(p2, p3)
    }
  }

  def setNumberLoops (): Int = {
    print(BLUE + "Planner" + GREEN + " : How many games do you want to execute ? > " + RESET)
    val newLoop = scala.io.StdIn.readLine()
    try {
      val newLoopToInt = newLoop.toInt
      if (newLoopToInt < 1) {
        println(RED + "\nERROR : " + newLoopToInt +" => Invalid number." + RESET)
        setNumberLoops()
      }
      else {
        println(newLoop + "\n")
        newLoop.toInt
      }
    }
    catch {
      case _: Throwable =>
        println(RED + "\nERROR : " + newLoop +" => Invalid number. Try again." + RESET)
        setNumberLoops()
    }
  }

  def updateAIHistory (AIName: String, h: Array[History], nbTestAI: Int): Array[History] = {
    AIName match {
      case "AI1" =>
        if (nbTestAI == 3) {
          // AI1 vs AI2
          val newH = h(0).copy(p1V = h(0).p1V + 1)
          val newArrayH = Array(newH, h(1), h(2))
          newArrayH
        } else {
          // AI1 vs AI3
          val newH = h(1).copy(p1V = h(1).p1V + 1)
          val newArrayH = Array(h(0), newH, h(2))
          newArrayH
        }

      case "AI2" =>
        if (nbTestAI == 3) {
          // AI1 vs AI2
          val newH = h(0).copy(p2V = h(0).p2V + 1)
          val newArrayH = Array(newH, h(1), h(2))
          newArrayH
        } else {
          // AI2 vs AI3
          val newH = h(2).copy(p1V = h(2).p1V + 1)
          val newArrayH = Array(h(0), h(1), newH)
          newArrayH
        }

      case "AI3" =>
        if (nbTestAI == 2) {
          // AI1 vs AI3
          val newH = h(1).copy(p2V = h(1).p2V + 1)
          val newArrayH = Array(h(0), newH, h(2))
          newArrayH
        } else {
          // AI2 vs AI3
          val newH = h(2).copy(p2V = h(2).p2V + 1)
          val newArrayH = Array(h(0), h(1), newH)
          newArrayH
        }

    }
  }

  def exportCSV (h: Array[History]): Boolean = {
    val outputFile = new BufferedWriter(new FileWriter("ai_proof.csv"))
    val csvWriter = new CSVWriter(outputFile)
    val csvFields: Array[String] = Array("AI Name", "score", "AI Name2", "score2")
    val row1: Array[String] = Array("AI Level Beginner", h(0).p1V.toString(), "Level Medium", h(0).p2V.toString())
    val row2: Array[String] = Array("AI Level Beginner", h(1).p1V.toString(), "Level Hard", h(1).p2V.toString())
    val row3: Array[String] = Array("AI Medium", h(1).p1V.toString(), "Level Hard", h(2).p2V.toString())

    var listOfRecords = new ListBuffer[Array[String]]()
    listOfRecords += csvFields
    listOfRecords += row1
    listOfRecords += row2
    listOfRecords += row3

    csvWriter.writeAll(listOfRecords.toList)
    outputFile.close()

    true
  }
}
