package revisions

import org.scalatest.FunSuite

import RevisionExecutor.Implicits.global


class VersionedValueSpec extends FunSuite {

  test("initialize and retrieve a versioned value") {
    val v = VersionedValue(0)

    assert(v.value === 0)
  }

  test("initialize, set, and retrieve a versioned value") {
    val v = VersionedValue(0)

    v.value = 1

    assert(v.value === 1)
  }

  test("a join uses the value of the branch") {
    val v = VersionedValue(0)

    val r = Revision.fork {
      assert(v.value === 0)

      v.value = 1
    }

    assert(v.value === 0)

    Revision.join(r)

    assert(v.value === 1)
  }

  test("a revision branch can only be joined once") {
    val v = VersionedValue(0)

    val r = Revision.fork {
      v.value = 1
    }

    Revision.join(r)

    intercept[AssertionError] {
      Revision.join(r)
    }
  }

  test("a join overwrites") {
    val v = VersionedValue(0)

    val r = Revision.fork {
      assert(v.value === 0)

      v.value = 2
    }
    v.value = 1

    assert(v.value === 1)

    Revision.join(r)

    assert(v.value === 2)
  }

  test("independent writes to independent versioned values") {
    val x = VersionedValue(0)
    val y = VersionedValue(0)

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

  test("versioned value defined in branch") {
    val x = VersionedValue(0)

    val r = Revision.fork {
      x.value = 1

      assert(x.value === 1)

      val y = VersionedValue(0)

      assert(y.value === 0)

      y
    }

    assert(x.value === 0)

    val y = Revision.join(r)

    assert(x.value === 1)
    assert(y.value === 0)
  }

  test("fork r1; fork r2; join r1; join r2") {
    val x = VersionedValue(5)
    val y = VersionedValue(7)

    val r1 = Revision.fork {
      if (x.value == 5) y.value = 1

      assert(y.value === 1)
    }

    val r2 = Revision.fork {
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

  test("fork r1; fork r2; join r2; join r1") {
    val x = VersionedValue(5)
    val y = VersionedValue(7)

    val r1 = Revision.fork {
      if (x.value == 5) y.value = 1

      assert(y.value === 1)
    }

    val r2 = Revision.fork {
      if (y.value == 7) x.value = 10

      assert(x.value === 10)
    }

    assert(x.value === 5)
    assert(y.value === 7)

    Revision.join(r2)

    assert(x.value === 10)
    assert(y.value === 7)

    Revision.join(r1)

    assert(x.value === 10)
    assert(y.value === 1)
  }

  test("fork r1; join r1; fork r2; join r2") {
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

  test("fork and join a branch inside a branch") {
    val x = VersionedValue(5)
    val y = VersionedValue(7)

    val r1 = Revision.fork {

      val r2 = Revision.fork {
        assert(x.value === 5)
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

      val r2 = Revision.fork {

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

  test("deep nesting with set, joining inner branches inside outer branches") {
    val x = VersionedValue(0)

    val r1 = Revision.fork {

      val r2 = Revision.fork {

        val r3 = Revision.fork {

          val r4 = Revision.fork {
            x.value = 1
          }

          assert(x.value === 0)

          Revision.join(r4)

          assert(x.value === 1)
        }

        assert(x.value === 0)

        Revision.join(r3)

        assert(x.value === 1)
      }

      assert(x.value === 0)

      Revision.join(r2)

      assert(x.value === 1)
    }

    assert(x.value === 0)

    Revision.join(r1)

    assert(x.value === 1)
  }

  test("deep nesting with set, joining inner branches after outer branches") {
    val x = VersionedValue(0)

    val r1 = Revision.fork {

      val r2 = Revision.fork {

        val r3 = Revision.fork {

          val r4 = Revision.fork {
            x.value = 1
          }

          x.value = 2

          r4
        }

        x.value = 3

        r3
      }

      x.value = 4

      r2
    }

    x.value = 5

    assert(x.value === 5)

    val r2 = Revision.join(r1)

    assert(x.value === 4)

    val r3 = Revision.join(r2)

    assert(x.value === 3)

    val r4 = Revision.join(r3)

    assert(x.value === 2)

    Revision.join(r4)

    assert(x.value === 1)
  }

  test("deep nesting with accumulate, joining inner branches after outer branches") {
    val x = VersionedValue(0)

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

    x.value += 1

    assert(x.value === 1)

    val r2 = Revision.join(r1)

    assert(x.value === 2)

    val r3 = Revision.join(r2)

    assert(x.value === 3)

    val r4 = Revision.join(r3)

    assert(x.value === 4)

    Revision.join(r4)

    assert(x.value === 4)
  }

  test("transport inner branch across outer branches") {
    val x = VersionedValue(0)

    val r1 = Revision.fork {
      x.value = 1

      val r2 = Revision.fork {
        x.value = 2
      }

      r2
    }

    assert(x.value === 0)

    val r2 = Revision.join(r1)

    assert(x.value === 1)

    val r3 = Revision.fork {
      x.value = 3

      assert(x.value === 3)

      Revision.join(r2)

      assert(x.value === 2)
    }

    Revision.join(r3)

    assert(x.value === 2)
  }

  test("fork i; fork i+1; join i into i+1") {
    val x = VersionedValue(0)

    val r1 = Revision.fork {
      x.value = 1

      assert(x.value === 1)
    }

    val r2 = Revision.fork {
      x.value = 2

      assert(x.value === 2)

      Revision.join(r1)

      assert(x.value === 1)
    }

    val r3 = Revision.fork {
      x.value = 3

      assert(x.value === 3)

      Revision.join(r2)

      assert(x.value === 1)
    }

    val r4 = Revision.fork {
      x.value = 4

      assert(x.value === 4)

      Revision.join(r3)

      assert(x.value === 1)
    }

    assert(x.value === 0)

    Revision.join(r4)

    assert(x.value === 1)
  }

  test("fork i; fork i+1; fork i+2; join i into i+2; fork i+3; join i+1 into i+3") {
    val x = VersionedValue(0)

    val r1 = Revision.fork {
      x.value = 1

      assert(x.value === 1)
    }

    val r2 = Revision.fork {
      x.value = 2

      assert(x.value === 2)
    }

    val r3 = Revision.fork {
      x.value = 3

      assert(x.value === 3)

      Revision.join(r1)

      assert(x.value === 1)
    }

    val r4 = Revision.fork {
      x.value = 4

      assert(x.value === 4)

      Revision.join(r2)

      assert(x.value === 2)
    }

    assert(x.value === 0)

    Revision.join(r3)

    assert(x.value === 1)

    Revision.join(r4)

    assert(x.value === 2)
  }

  test("illegal attempt to merge inner branch before outer branch") {
    val x = VersionedValue(0)

    @volatile
    var r2: Revision[_] = null

    val r1 = Revision.fork {
      x.value = 1

      r2 = Revision.fork {
        x.value = 2
      }
    }

    while (r2 eq null) { }

    intercept[AssertionError] {
      Revision.join(r2)
    }
  }

  test("illegal attempt to crisscross branches") {
    val x = VersionedValue(0)

    @volatile
    var r3: Revision[_] = null

    @volatile
    var r4: Revision[_] = null

    val r1 = Revision.fork {
      x.value = 1

      r3 = Revision.fork {
        x.value = 3
      }

      while (r4 eq null) { }

      intercept[AssertionError] {
        Revision.join(r4)
      }
    }

    val r2 = Revision.fork {
      x.value = 2

      r4 = Revision.fork {
        x.value = 4
      }

      while (r3 eq null) { }

      intercept[AssertionError] {
        Revision.join(r3)
      }
    }

    Revision.join(r1)
    Revision.join(r2)
  }

}
