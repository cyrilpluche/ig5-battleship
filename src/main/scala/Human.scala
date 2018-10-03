import Console.{BLUE, GREEN, RED, YELLOW, RESET, MAGENTA, UNDERLINED}

case class Human (name: String, c: Int, isIA: Boolean) extends Player (name, c, isIA) {

  /* PLAY METHODS */
  def play (): Array[Int] = {
    /* Console display */
    print(colors(c) + name + GREEN + " : " + "Choose a col letter > " + RESET)
    val a1: String = scala.io.StdIn.readLine()
    println(a1)
    print(colors(c) + name + GREEN + " : " + "Choose a row number > " + RESET)
    val a2 = scala.io.StdIn.readLine()
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
      Array(a2.toInt, alphabet.indexOf(a1))
    }
    else {
      println("\n" + RED + "Please, submit a valid location.\n")
      play()
    }
  }

  /* PLACE SHIP METHODS */
  def placeShip (size: Int): (Array[Int], String) = {
    println(YELLOW + "Submit a location for the first slot of the ship of size " + size + "." + RESET)
    val origin = play()

    /* Console display */
    println(YELLOW + "Submit an orientation for the ship (right, left, top, bottom)." + RESET)
    print(colors(c) + name + GREEN + " : " + "Choose an orientation > " + RESET)
    val a1 = scala.io.StdIn.readLine()
    println(a1 + "\n")

    if (checkPlaceShipInputs(a1)) {
      (origin, a1)
    }
    else {
      println("\n" + RED + "Please, submit valid ship informations.\n")
      placeShip(size)
    }
  }

  // def chooseASlot (grid: Array[Array[Boolean]], r: Random, slot: Array[Int], firstSlot: Array[Int]) : Array[Int]

}

