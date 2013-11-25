package revisions

import org.scalatest.FunSuite

import RevisionExecutor.Implicits.global


class CumulativeValueSpec extends FunSuite {

  val addMerge = (root: Int, main: Int, join: Int) => main + join - root

  test("cumulative single branch") {
    val x = CumulativeValue(0, addMerge)

    val r1 = Revision.fork {
      assert(x.value === 0)

      x.value += 2

      assert(x.value === 2)
    }

    x.value += 3

    assert(x.value === 3)

    Revision.join(r1)

    assert(x.value === 5)
  }

  test("inner branch merged after outer brach with cumulative value") {
    val x = CumulativeValue(0, addMerge)

    val r1 = Revision.fork {
      assert(x.value === 0)

      x.value += 1

      assert(x.value === 1)

      val r2 = Revision.fork {
        assert(x.value === 1)

        x.value += 3

        assert(x.value === 4)
      }

      assert(x.value === 1)

      r2
    }

    x.value += 2

    assert(x.value === 2)

    val r2 = Revision.join(r1)

    assert(x.value === 3)

    x.value += 4

    assert(x.value === 7)

    Revision.join(r2)

    assert(x.value === 10)
  }

  val maxMerge = (root: Int, main: Int, join: Int) => math.max(main, join)
  val minMerge = (root: Int, main: Int, join: Int) => math.min(main, join)

  test("mixed merge functions") {
    val x = CumulativeValue(0, addMerge)
    val y = CumulativeValue(0, maxMerge)
    val z = CumulativeValue(0, minMerge)

    val r = Revision.fork {
      assert(x.value === 0)
      assert(y.value === 0)
      assert(z.value === 0)

      x.value += 5
      y.value = 10
      z.value = -5

      assert(x.value === 5)
      assert(y.value === 10)
      assert(z.value === -5)
    }

    assert(x.value === 0)
    assert(y.value === 0)
    assert(z.value === 0)

    x.value += 3
    y.value = 5
    z.value = -10

    assert(x.value === 3)
    assert(y.value === 5)
    assert(z.value === -10)

    Revision.join(r)

    assert(x.value === 8)
    assert(y.value === 10)
    assert(z.value === -10)

  }
}
