import scala.util.Random
import Console.{BLUE, GREEN, RED, YELLOW, RESET, YELLOW_B, UNDERLINED}

case class Player (name: String, c: Int) {

  private val colors: Array[Any] = Array(Console.BLUE, Console.MAGENTA)
  private val alphabet: Array[String] = Array("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
  private val orientations: Array[String] = Array("right", "left", "top", "bottom")

  /* PLAY METHODS */
  def play (): Any = {
    /* Console display */
    val a1 = readLine(colors(c) + name + GREEN + " : " + "Choose a row letter > " + RESET)
    println(a1)
    val a2 = readLine(colors(c) + name + GREEN + " : " + "Choose a col number > " + RESET)
    println(a2)

    /* Parameters checking */
    var isValid: Boolean = true
    try {
      val a2ToInt = a2.toInt
    }
    catch {
      case _: Throwable =>
        println(RED + "ERROR : " + a2 +" => Invalid col." + RESET)
        isValid = false
    }

    if (isValid && checkPlayInputs(a1, a2.toInt)) {
      println("")
      (alphabet.indexOf(a1), a2.toInt)
    }
    else {
      println("\n" + RED + "Please, submit a valid location.\n")
      play()
    }
  }

  private def checkPlayInputs (a1: String, a2: Int): Boolean = {
    if (!alphabet.contains(a1)) {
      println(RED + "ERROR : " + a1 +" => Invalid letter." + RESET)
      false
    }
    else if (a2 < 0) {
      println(RED + "ERROR : " + a2 +" => Negativ col." + RESET)
      false
    }
    else true
  }

  /* PLACE SHIP METHODS */
  def placeShip (size: Int): Any = {
    println(YELLOW + "Submit a location for the first slot of the ship." + RESET)
    val origin = play()

    /* Console display */
    println(YELLOW + "Submit an orientation for the ship (right, left, top, bottom)." + RESET)
    val a1 = readLine(colors(c) + name + GREEN + " : " + "Choose an orientation > " + RESET)
    println(a1)

    if (checkPlaceShipInputs(a1)) {
      println("")
      (origin, a1)
    }
    else {
      println("\n" + RED + "Please, submit valid ship informations.\n")
      placeShip(size)
    }
  }

  private def checkPlaceShipInputs (a1: String): Boolean = {
    if (!orientations.contains(a1)) {
      println(RED + "ERROR : " + a1 +" => Invalid orientation." + RESET)
      false
    }
    else true
  }

  // def chooseASlot (grid: Array[Array[Boolean]], r: Random, slot: Array[Int], firstSlot: Array[Int]) : Array[Int]

}
