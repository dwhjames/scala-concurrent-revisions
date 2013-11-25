package revisions

import org.scalatest.FunSuite

import RevisionExecutor.Implicits.global


class ReadAndWriteSkewSpec extends FunSuite {

  type Account = CumulativeValue[(Int, Int)]

  def createAccount(checking: Int, savings: Int): Account =
    new CumulativeValue[(Int, Int)]((checking, savings)) {
      override def mergeValue(root: (Int, Int), main: (Int, Int), join: (Int, Int)): (Int, Int) = {
        val newChecking = main._1 + join._1 - root._1
        val newSavings  = main._2 + join._2 - root._2
        if ((newChecking + newSavings < 0) &&
            (main._1 + main._2 >= 0))
          (newChecking - 1, newSavings)
        else
          (newChecking, newSavings)
      }
    }

  def depositChecking(acc: Account, i: Int): Unit = {
    val (c, s) = acc.value
    acc.value = (c + i, s)
  }

  def depositSavings(acc: Account, i: Int): Unit = {
    val (c, s) = acc.value
    acc.value = (c, s + i)
  }

  def balance(acc: Account): (Int, Int) =
    acc.value


  test("read skew") {
    val acc = createAccount(0, 0)

    val r1 = Revision.fork {
      depositSavings(acc, 20)
    }

    val r2 = Revision.fork {
      depositChecking(acc, -10)
    }

    Revision.join(r1)

    val r3 = Revision.fork {
      balance(acc)
    }

    val bal = Revision.join(r3)
    assert(bal === (0, 20))

    Revision.join(r2)

    assert(acc.value === (-10, 20))
  }

  test("write skew") {
    val acc = createAccount(70, 80)

    val r1 = Revision.fork {
      depositSavings(acc, -100)
    }

    val r2 = Revision.fork {
      depositChecking(acc, -100)
    }

    Revision.join(r1)
    Revision.join(r2)

    assert(acc.value === (-31, -20))
  }

}
