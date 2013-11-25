package revisions

import org.scalatest.FunSuite

import RevisionExecutor.Implicits.global


class CounterSpec extends FunSuite {

  type Counter = CumulativeValue[Int]

  def createCounter(i: Int): Counter =
    CumulativeValue(i, (root: Int, main: Int, join: Int) =>
      main + join - root
    )


  test("counter") {
    val c = createCounter(0)

    val r1 = Revision.fork {
      c.value += 1

      val r2 = Revision.fork {
        c.value += 3
      }

      c.value += 2

      r2
    }

    assert(c.value === 0)

    c.value += 4

    assert(c.value === 4)

    val r2 = Revision.join(r1)

    assert(c.value === 7)

    Revision.join(r2)

    assert(c.value === 10)
  }

}
