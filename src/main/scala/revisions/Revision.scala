package revisions

import java.util.concurrent.{ExecutorService, Future}


class Revision(
    private[revisions] val root: Segment,
    private[revisions] var current: Segment
) {
  private var task: Future[_] = _

  def fork(action: => Unit)(implicit execServ: ExecutorService): Revision = {
    val r = new Revision(current, new Segment(current))

    assert(current.refCount.get() > 1)
    current.release() // cannot bring refcount to zero

    current = new Segment(current)

    r.task = execServ.submit(new Runnable {
      override def run(): Unit = {
        val previous = Revision.currentRevision.get()
        Revision.currentRevision.set(r)
        try {
          action
        } finally {
          Revision.currentRevision.set(previous)
        }
      }
    })
    r
  }

  def join(join: Revision): Unit = {
    try {
      join.task.get()
      var s = join.current
      while (s ne join.root) {
        for (v <- s.written) {
          v.merge(this, join, s)
        }
        s = s.parent
      }
    } finally {
      join.current.release()
      current.collapse(this)
    }
  }
}

object Revision {
  val currentRevision = new ThreadLocal[Revision] {
    override def initialValue(): Revision = {
      val s = new Segment
      new Revision(s, s)
    }
  }

  def currentVersion = currentRevision.get().current.version

  def fork(action: => Unit)(implicit execServ: ExecutorService): Revision =
    currentRevision.get().fork(action)

  def join(join: Revision): Unit =
    currentRevision.get().join(join)
}
