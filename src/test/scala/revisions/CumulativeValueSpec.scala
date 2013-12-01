package revisions

import org.scalatest.FunSuite

import RevisionExecutor.Implicits.global


class CumulativeValueSpec extends FunSuite {

  val addMerge = (root: Int, main: Int, join: Int) => main + join - root

  test("initialize and retrieve a cumulative value") {
    val v = CumulativeValue(0, addMerge)

    assert(v.value === 0)
  }

  test("initialize, set, and retrieve a cumulative value") {
    val v = CumulativeValue(0, addMerge)

    v.value = 1

    assert(v.value === 1)
  }

  test("cumulative single branch") {
    val x = CumulativeValue(1, addMerge)

    val r1 = Revision.fork {
      assert(x.value === 1)

      x.value += 2

      assert(x.value === 3)
    }

    x.value += 3

    assert(x.value === 4)

    Revision.join(r1)

    assert(x.value === 6)
  }

  test("a revision branch can only be joined once") {
    val v = CumulativeValue(0, addMerge)

    val r = Revision.fork {
      v.value = 1
    }

    Revision.join(r)

    intercept[AssertionError] {
      Revision.join(r)
    }
  }

  test("independent writes to independent versioned values") {
    val x = CumulativeValue(0, addMerge)
    val y = CumulativeValue(0, addMerge)

    val r = Revision.fork {
      x.value = 1

      assert(x.value === 1)
    }

    y.value = x.value + 1

    assert(y.value === 1)

    Revision.join(r)

    assert(x.value === 1)
    assert(y.value === 1)
  }

  test("cumulative defined in branch") {
    val x = CumulativeValue(0, addMerge)

    val r1 = Revision.fork {
      assert(x.value === 0)

      x.value += 2

      assert(x.value === 2)

      val y = CumulativeValue(1, addMerge)

      assert(y.value === 1)

      y
    }

    x.value += 3

    assert(x.value === 3)

    val y = Revision.join(r1)

    assert(x.value === 5)
    assert(y.value === 1)
  }

  test("fork r1; fork r2; join r1; join r2") {
    val x = CumulativeValue(5, addMerge)

    val r1 = Revision.fork {
      x.value += 1

      assert(x.value === 6)
    }

    x.value += 3

    val y = CumulativeValue(7, addMerge)

    val r2 = Revision.fork {
      y.value += 2

      assert(y.value === 9)
    }

    assert(x.value === 8)
    assert(y.value === 7)

    Revision.join(r1)

    assert(x.value === 9)
    assert(y.value === 7)

    Revision.join(r2)

    assert(x.value === 9)
    assert(y.value === 9)
  }

  test("fork r1; fork r2; join r2; join r1") {
    val x = CumulativeValue(5, addMerge)

    val r1 = Revision.fork {
      x.value += 1

      assert(x.value === 6)
    }

    x.value += 3

    val y = CumulativeValue(7, addMerge)

    val r2 = Revision.fork {
      y.value += 2

      assert(y.value === 9)
    }

    assert(x.value === 8)
    assert(y.value === 7)

    Revision.join(r2)

    assert(x.value === 8)
    assert(y.value === 9)

    Revision.join(r1)

    assert(x.value === 9)
    assert(y.value === 9)
  }

  test("fork and join a branch inside a branch") {
    val x = CumulativeValue(5, addMerge)
    val y = CumulativeValue(7, addMerge)

    val r1 = Revision.fork {
      x.value += 1
      y.value += 1

      val r2 = Revision.fork {
        assert(x.value === 6)
        assert(y.value === 8)

        x.value += 4

        assert(x.value === 10)
      }

      x.value += 1
      y.value += 1

      assert(x.value === 7)
      assert(y.value === 9)

      Revision.join(r2)

      assert(x.value === 11)
      assert(y.value === 9)
    }

    x.value += 1
    y.value += 1

    assert(x.value === 6)
    assert(y.value === 8)

    Revision.join(r1)

    assert(x.value === 12)
    assert(y.value === 10)
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

  test("deep nesting with accumulation, joining inner branches inside outer branches") {
    val x = CumulativeValue(1, addMerge)

    val r1 = Revision.fork {
      x.value += 1

      val r2 = Revision.fork {
        x.value += 1

        val r3 = Revision.fork {
          x.value += 1

          val r4 = Revision.fork {
            x.value += 1
          }

          assert(x.value === 4)

          Revision.join(r4)

          assert(x.value === 5)
        }

        assert(x.value === 3)

        Revision.join(r3)

        assert(x.value === 5)
      }

      assert(x.value === 2)

      Revision.join(r2)

      assert(x.value === 5)
    }

    assert(x.value === 1)

    Revision.join(r1)

    assert(x.value === 5)
  }

  test("deep nesting with accumulation, joining inner branches after outer branches") {
    val x = CumulativeValue(1, addMerge)

    val r1 = Revision.fork {
      x.value += 1

      val r2 = Revision.fork {
        x.value += 1

        val r3 = Revision.fork {
          x.value += 1

          val r4 = Revision.fork {
            x.value += 1
          }

          x.value += 1

          r4
        }

        x.value += 1

        r3
      }

      x.value += 1

      r2
    }

    assert(x.value === 1)

    x.value += 1

    assert(x.value === 2)

    val r2 = Revision.join(r1)

    assert(x.value === 4)

    val r3 = Revision.join(r2)

    assert(x.value === 6)

    val r4 = Revision.join(r3)

    assert(x.value === 8)

    Revision.join(r4)

    assert(x.value === 9)
  }

  test("transport inner branch across outer branches") {
    val x = CumulativeValue(1, addMerge)

    val r1 = Revision.fork {
      x.value += 1

      val r2 = Revision.fork {
        x.value += 2
      }

      r2
    }

    x.value += 1

    assert(x.value === 2)

    val r2 = Revision.join(r1)

    assert(x.value === 3)

    val r3 = Revision.fork {
      x.value += 3

      assert(x.value === 6)

      Revision.join(r2)

      assert(x.value === 8)
    }

    x.value += 1

    Revision.join(r3)

    assert(x.value === 9)
  }

  test("fork i; fork i+1; join i into i+1") {
    val x = CumulativeValue(1, addMerge)

    val r1 = Revision.fork {
      x.value += 1

      assert(x.value === 2)
    }

    x.value += 1

    val r2 = Revision.fork {
      x.value += 2

      assert(x.value === 4)

      Revision.join(r1)

      assert(x.value === 5)
    }

    x.value += 1

    val r3 = Revision.fork {
      x.value += 3

      assert(x.value === 6)

      Revision.join(r2)

      assert(x.value === 9)
    }

    x.value += 1

    val r4 = Revision.fork {
      x.value += 4

      assert(x.value === 8)

      Revision.join(r3)

      assert(x.value === 14)
    }

    x.value += 1

    assert(x.value === 5)

    Revision.join(r4)

    assert(x.value === 15)
  }

  test("fork i; fork i+1; fork i+2; join i into i+2; fork i+3; join i+1 into i+3") {
    val x = CumulativeValue(1, addMerge)

    val r1 = Revision.fork {
      x.value += 1

      assert(x.value === 2)
    }

    x.value += 1

    val r2 = Revision.fork {
      x.value += 2

      assert(x.value === 4)
    }

    x.value += 1

    val r3 = Revision.fork {
      x.value += 3

      assert(x.value === 6)

      Revision.join(r1)

      assert(x.value === 7)
    }

    x.value += 1

    val r4 = Revision.fork {
      x.value += 4

      assert(x.value === 8)

      Revision.join(r2)

      assert(x.value === 10)
    }

    x.value += 1

    assert(x.value === 5)

    Revision.join(r3)

    assert(x.value === 9)

    Revision.join(r4)

    assert(x.value === 15)
  }

  test("illegal attempt to merge inner branch before outer branch") {
    val x = CumulativeValue(1, addMerge)

    @volatile
    var r2: Revision[_] = null

    val r1 = Revision.fork {
      x.value += 1

      r2 = Revision.fork {
        x.value += 2
      }
    }

    while (r2 eq null) { }

    intercept[AssertionError] {
      Revision.join(r2)
    }
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
