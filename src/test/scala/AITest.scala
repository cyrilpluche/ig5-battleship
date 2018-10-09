import org.scalatest.FunSuite

import scala.annotation.tailrec
import scala.util.Random

class AITest extends FunSuite {
  val ai1 = AI1(new Random, "AI1", 0, true, 1)
  val ai2 = new AI2(new Random, "AI2", 0, true, 2)
  val ai3 = new AI3(new Random, "AI3", 0, true, 3)

  val test1 = playTest(ai1, 100, 0)
  val test2 = placeShipTest(ai1, Array(2, 3, 3, 4, 5), 0)
  val test3 = playTest(ai2, 100, 0)
  val test4 = placeShipTest(ai2, Array(2, 3, 3, 4, 5), 0)
  val test5 = playTest(ai3, 100, 0)
  val test6 = placeShipTest(ai3, Array(2, 3, 3, 4, 5), 0)

  def playTest (player: Player, nbRandom: Int, i: Int): Boolean = {
    if (i == nbRandom) true
    else {
      test(player.getName() + ".play/" + i) {
        val xy = player.play()
        val y = xy(0)
        val x = xy(1)
        assert(y < 10 && y > -1 && x < 10 && x > -1)
      }
      playTest(player, nbRandom, i + 1)
    }
  }

  def placeShipTest (player: Player, size: Array[Int], i: Int): Boolean = {
    if (i == size.size) true
    else {
      test(player.getName() + ".placeShip/" + i.toString()) {
        val ship = player.placeShip(size(i))
        val origin = ship._1
        val orientation = ship._2
        assert(origin(0) < 10 && origin(0) > -1 && origin(1) < 10 && origin(1) > -1)
        assert(orientation == "right" || orientation == "left" || orientation == "bottom" || orientation == "top")
      }
      placeShipTest(player, size, i + 1)
    }
  }
}