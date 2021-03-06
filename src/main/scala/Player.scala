import scala.util.Random
import Console.{BLUE, GREEN, RED, YELLOW, RESET, MAGENTA, UNDERLINED}

abstract class Player (name: String, c: Int, isAI: Boolean, lvlAI: Int) {

  protected val colors: Array[Any] = Array(BLUE, MAGENTA)
  protected val alphabet: Array[String] = Array("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
  protected val orientations: Array[String] = Array("right", "left", "top", "bottom")

  /* Var for IA3 only */
  var lastShot: Array[Int] = Array(0, 0)
  var isShipTouched: Int = 0
  var orientationChosen: String = "right"
  var nbOrientationTried: Int = 4
  var targetLocked: Boolean = false

  /* PLAY METHODS */
  def play (): Array[Int]

  def checkPlayInputs (a1: String, a2: Int): Boolean = {
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
  def placeShip (size: Int): (Array[Int], String)

  def checkPlaceShipInputs (a1: String): Boolean = {
    if (!orientations.contains(a1)) {
      println(RED + "ERROR : " + a1 +" => Invalid orientation." + RESET)
      false
    }
    else true
  }

  def getColor (): Any = {
    colors(c)
  }

  def getName (): String = {
    name
  }

  def getIsAI (): Boolean = {
    isAI
  }

  def getLvlAI (): Int = {
    lvlAI
  }

}
