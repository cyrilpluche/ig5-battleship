import Console.{BLUE, GREEN, MAGENTA, RED, RESET, UNDERLINED, YELLOW}

object Helper {

  def displayToUser (player: Option[Player], opponent: Option[Player], msg: String, style: Int, space: Boolean) = {
    if (!player.get.getIsAI()) {
      style match {
        case 0 =>
          /* Game state */
          println(RED + "========== " + msg + " ==========" + RESET)

        case 1 =>
          /* Text from the speaker */
          println(YELLOW + msg + RESET)

        case 2 =>
          /* Text for a player */
          println(player.get.getColor() + player.get.getName() + YELLOW + " : " + msg + RESET)

        case 3 =>
          /* Text from speaker for a player */
          println(player.get.getColor() + player.get.getName() + " " + GREEN + msg + RESET)
        case 4 =>
          /* Error */
          println(RED + msg + RESET)
        case 5 =>
          /* Text for a IA vs player */
          println(player.get.getColor() + player.get.getName() + YELLOW + " : " + msg + RESET)

      }
      if (space) println("")
    } else {

      try {
        if (!player.get.getIsAI() || !opponent.get.getIsAI()) {
          style match {
            case 2 =>
              /* Text for a IA vs player */
              println(player.get.getColor() + player.get.getName() + YELLOW + " : " + msg + RESET)

            case 3 =>
              /* Text from speaker for a player */
              println(player.get.getColor() + player.get.getName() + " " + GREEN + msg + RESET)
          }
          if (space) println("")
        }

      }
      catch {
        case _: Throwable =>
          // nothing
      }


    }
  }

}
