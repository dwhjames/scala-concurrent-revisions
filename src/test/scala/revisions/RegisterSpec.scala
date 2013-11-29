package revisions

import org.scalatest.FunSuite

import RevisionExecutor.Implicits.global


class RegisterSpec extends FunSuite {

  sealed trait Reg {
    def get: Int
    def add(i: Int): Reg
    def fork: Reg
  }

  case class Rel(base: Int, x: Int) extends Reg {
    override def get = base + x
    override def add(i: Int) = Rel(base, x + i)
    override def fork = Rel(base + x, 0)
  }

  case class Abs(x: Int) extends Reg {
    override def get = x
    override def add(i: Int) = Abs(x + i)
    override def fork = Rel(x, 0)
  }

  type Register = MergeForkValue[Reg]

  def get(r: Register): Int =
    r.get.get

  def add(r: Register, i: Int): Unit =
    r.modify(_.add(i))

  def set(r: Register, i: Int): Unit =
    r.modify(_ => Abs(i))

  def createRegister: Register =
    MergeForkValue(
      Abs(0),
      (root: Reg, main: Reg, join: Reg) =>
        join match {
          case Abs(x) => Abs(x)
          case Rel(_, x) =>
            main match {
              case Rel(base, y) => Rel(base, x + y)
              case Abs(y) => Abs(x + y)
            }
        },
      (x: Reg) => x.fork
    )


  test("register") {
    val r = createRegister

    assert(get(r) === 0)

    val r1 = Revision.fork {
      add(r, 1)

      assert(get(r) === 1)

      val r2 = Revision.fork {
        add(r, 1)

        assert(get(r) === 2)
      }

      assert(get(r) === 1)

      add(r, 1)

      assert(get(r) === 2)

      r2
    }

    set(r, 2)

    assert(get(r) === 2)

    val r2 = Revision.join(r1)

    assert(get(r) === 4)

    Revision.join(r2)

    assert(get(r) === 5)
  }

}
