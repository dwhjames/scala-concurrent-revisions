
import revisions._
import RevisionExecutor.Implicits.global

import scala.util.Random

import java.util.Scanner
import java.util.concurrent.ThreadLocalRandom

object Asteroids extends App {

  type Pos = (Int, Int)

  type Player = VersionedValue[Pos]
  type Asteroid = VersionedValue[Pos]

  val maxX: Int = 5
  val maxY: Int = 4

  val coords: Seq[Pos] =
    for {
      x <- 0 to maxX
      y <- 0 to maxY
    } yield (x, y)

  val initialPlayer: Pos = (2, 2)

  val initialAsteroids: Seq[Pos] = Seq(
      (0, 0),
      (1, 1),
      (2, 3),
      (4, 4)
    )

  def left (p: Pos): Pos = (p._1 - 1, p._2)
  def right(p: Pos): Pos = (p._1 + 1, p._2)
  def up   (p: Pos): Pos = (p._1,     p._2 - 1)
  def down (p: Pos): Pos = (p._1,     p._2 + 1)

  def move(versionedPos: VersionedValue[Pos], adjust: Pos => Pos): Unit = {
    val old = versionedPos.value
    val (x, y) = adjust(old)
    if (x >= 0 && y >= 0 && x <= maxX && y <= maxY)
      versionedPos.value = (x, y)
  }

  def playerTurn(player: Player): Unit = {
    val sc = new Scanner(System.in)
    val line = blocking { sc.nextLine() }
    line match {
      case "l" => move(player, left)
      case "r" => move(player, right)
      case "u" => move(player, up)
      case "d" => move(player, down)
      case _ =>
    }
  }

  def asteroidTurn(asteroid: Asteroid): Unit = {
    val r = new Random(ThreadLocalRandom.current())
    val shouldMove = r.nextBoolean()
    if (shouldMove) {
      val dir = r.nextInt(4)
      dir match {
        case 0 => move(asteroid, left)
        case 1 => move(asteroid, right)
        case 2 => move(asteroid, up)
        case 3 => move(asteroid, down)
      }
    }
  }

  def asteroidsTurn(asteroids: Seq[Asteroid]): Unit =
    asteroids foreach asteroidTurn

  def wormhole(asteroids: Seq[Asteroid]): Unit = {
    val r = new Random(ThreadLocalRandom.current())
    val createHole = r.nextBoolean()
    if (createHole) {
      val idx = r.nextInt(asteroids.size)
      val asteroid = asteroids(idx)
      val x = r.nextInt(maxX+1)
      val y = r.nextInt(maxY+1)
      asteroid.value = (x, y)
    }
  }

  def render(asteroids: Seq[Asteroid], player: Player): Unit = {
    val ps = asteroids.map(_.value)
    val p = player.value
    val builder = new StringBuilder((maxX + 1) * maxY + 1)
    for (y <- 0 to maxY) {
      for (x <- 0 to maxX) {
        if (p == (x, y))
          builder += 'P'
        else if (ps contains (x, y))
          builder += '*'
        else
          builder += '.'
      }
      builder += '\n'
    }
    builder += '\n'
    blocking { print(builder.result()) }
  }

  def play(asteroids: Seq[Asteroid], player: Player): Unit = {
    var continue = true
    do {
      val r1 = Revision.fork {
        playerTurn(player)
      }
      val r2 = Revision.fork {
        asteroidsTurn(asteroids)
      }
      val r3 = Revision.fork {
        wormhole(asteroids)
      }

      render(asteroids, player)

      Revision.join(r1)
      Revision.join(r2)
      Revision.join(r3)

      val ps = asteroids.map(_.value)
      val p = player.value

      if (ps contains p) {
        blocking { println("You hit an asteroid!") }
        continue = false
      } else if (p == (0,0)) {
        blocking { println("You won!") }
        continue = false
      }

    } while (continue)
  }

  val player = VersionedValue(initialPlayer)
  val asteroids = for (asteroid <- initialAsteroids) yield VersionedValue(asteroid)

  play(asteroids, player)
}
