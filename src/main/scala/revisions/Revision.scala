package revisions

import java.util.concurrent.{ExecutorService, Future}


class Revision(
    private[revisions] val root: Segment,
    private[revisions] var current: Segment
) {
  private var task: Future[_] = _

  def fork(action: => Unit)(implicit execServ: ExecutorService): Revision = {
    // construct a new revision with the current segment as the root
    // and a new segement as its current (where its parent is the root)
    val r = new Revision(current, new Segment(current))

    assert(current.refCount.get() > 1)
    current.release() // cannot bring refcount to zero

    // create new current segment with old current as its parent
    current = new Segment(current)

    // fill in the task for the new segment
    // submit the call by name argument to the executor service
    r.task = execServ.submit(new Runnable {
      override def run(): Unit = {
        // save the current revision on this thread
        val previous = Revision.currentRevision.get()
        // set the current revision on this thread to the revision we created
        Revision.currentRevision.set(r)
        try {
          action
        } finally {
          // restore the original revision
          Revision.currentRevision.set(previous)
        }
      }
    })
    r
  }

  def join(join: Revision): Unit = {
    try {
      // wait for the revision to join to complete
      join.task.get()

      // walk up its segment history
      var s = join.current
      while (s ne join.root) {
        for (v <- s.written) {
          // and merge everything that was written into this revision
          v.merge(this, join, s)
        }
        s = s.parent
      }
    } finally {
      // release the segments in the joined revision
      join.current.release()
      // collapse the segments in this revision
      current.collapse(this)
    }
  }
}

object Revision {
  // the revision for the current thread
  val currentRevision = new ThreadLocal[Revision] {
    override def initialValue(): Revision = {
      val s = new Segment
      new Revision(s, s)
    }
  }

  // diagnostic method to expose the version number of the current segment version
  // for the current thread’s revision
  def currentVersion = currentRevision.get().current.version

  // fork an action from the current thread’s revision
  def fork(action: => Unit)(implicit execServ: ExecutorService): Revision =
    currentRevision.get().fork(action)

  // join a revision into the current thread’s revision
  def join(join: Revision): Unit =
    currentRevision.get().join(join)
}
