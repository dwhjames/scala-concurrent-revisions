package revisions

import RevisionExecutor.Implicits.global


object Test {
  def main(args: Array[String]): Unit = {
    val x = new VersionedValue[Int](0)
    val y = new VersionedValue[Int](0)
    println("start (v%d): (x, y) == (%d, %d)".format(Revision.currentVersion, x.value, y.value))

    // println(Revision.currentRevision.get().current.version)

    val r = Revision.fork {
      if (x.value == 0) {
        y.value = y.value + 1
      }
      println("branch (v%d): (x, y) == (%d, %d)".format(Revision.currentVersion, x.value, y.value))
    }

    if (y.value == 0) {
      x.value = x.value + 1
    }
    println("main (v%d): (x, y) == (%d, %d)".format(Revision.currentVersion, x.value, y.value))

    Revision.join(r)

    println("end (v%d): (x, y) == (%d, %d)".format(Revision.currentVersion, x.value, y.value))

  }
}
