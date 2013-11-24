package revisions

import org.scalatest.FunSuite

import RevisionExecutor.Implicits.global


class VersionedValueSpec extends FunSuite {

  test("version value is initialized") {
    val v = VersionedValue(0)

    assert(v.value === 0)
  }

  test("a join overwrites") {
    val v = VersionedValue(0)

    assert(v.value === 0)

    val r = Revision.fork {
      assert(v.value === 0)

      v.value = 2
    }
    v.value = 1

    assert(v.value === 1)

    Revision.join(r)

    assert(v.value === 2)
  }

  test("deterministic single branch") {
    val x = VersionedValue(0)
    val y = VersionedValue(0)

    val r = Revision.fork {
      assert(x.value === 0)

      x.value = 1

      assert(x.value === 1)
    }

    assert(x.value === 0)

    y.value = x.value

    assert(y.value === 0)

    Revision.join(r)

    assert(x.value === 1)
    assert(y.value === 0)
  }

  test("deterministic double branch 1") {
    val x = VersionedValue(5)
    val y = VersionedValue(7)

    val r1 = Revision.fork {
      assert(x.value === 5)

      if (x.value == 5) y.value = 1

      assert(y.value === 1)
    }

    val r2 = Revision.fork {
      assert(y.value === 7)

      if (y.value == 7) x.value = 10

      assert(x.value === 10)
    }

    assert(x.value === 5)
    assert(y.value === 7)

    Revision.join(r1)

    assert(x.value === 5)
    assert(y.value === 1)

    Revision.join(r2)

    assert(x.value === 10)
    assert(y.value === 1)
  }

  test("deterministic double branch 2") {
    val x = VersionedValue(5)
    val y = VersionedValue(7)

    val r1 = Revision.fork {
      assert(x.value === 5)

      if (x.value == 5) y.value = 1

      assert(y.value === 1)
    }

    if (x.value == 5) y.value = 111

    assert(x.value === 5)
    assert(y.value === 111)

    Revision.join(r1)

    assert(x.value === 5)
    assert(y.value === 1)

    val r2 = Revision.fork {
      assert(x.value === 5)
      assert(y.value === 1)

      if (y.value == 1) x.value = 222

      assert(x.value === 222)
    }

    if (y.value == 1) x.value = 2

    assert(x.value === 2)
    assert(y.value === 1)

    Revision.join(r2)

    assert(x.value === 222)
    assert(y.value === 1)
  }

  test("deterministic nested branch") {
    val x = VersionedValue(5)
    val y = VersionedValue(7)

    val r1 = Revision.fork {
      assert(x.value === 5)

      val r2 = Revision.fork {
        assert(y.value === 7)

        if (y.value == 7) x.value = 10

        assert(x.value === 10)
      }

      if (x.value == 5) y.value = 1

      assert(y.value === 1)

      Revision.join(r2)

      assert(x.value === 10)
      assert(y.value === 1)
    }

    if (x.value == 5) y.value =  111

    assert(x.value === 5)
    assert(y.value === 111)

    Revision.join(r1)

    assert(x.value === 10)
    assert(y.value === 1)
  }

  test("inner branch merged after outer branch") {
    val x = VersionedValue(5)
    val y = VersionedValue(7)

    val r1 = Revision.fork {
      assert(x.value === 5)

      val r2 = Revision.fork {
        assert(y.value === 7)

        if (y.value == 7) x.value = 10

        assert(x.value === 10)
      }

      if (x.value == 5) y.value = 1

      assert(y.value === 1)

      r2
    }

    if (x.value == 5) y.value =  111

    assert(x.value === 5)
    assert(y.value === 111)

    val r2 = Revision.join(r1)

    assert(x.value === 5)
    assert(y.value === 1)

    Revision.join(r2)

    assert(x.value === 10)
    assert(y.value === 1)
  }

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
