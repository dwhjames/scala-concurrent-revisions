package revisions

import java.util.concurrent.{Callable, ExecutorService, Future}


class Revision[T](
    private[revisions] val root: Segment,
    private[revisions] var current: Segment
) {
  private var task: Future[T] = _

  def fork[S](action: => S)(implicit execServ: ExecutorService): Revision[S] = {
    // construct a revision for the new branch
    val r =
      if (current.written.isEmpty) {
        // if there have been no writes in the current segment of the
        // main revision, then we can reüse it
        new Revision[S](current.parent, new Segment(current.parent))
      } else {
        // construct a new revision with the current segment as the root
        // and a new segement as its current (where its parent is the root)
        val _r = new Revision[S](current, new Segment(current))

        // release the current segment in the main revision
        assert(current.refCount.get() > 1)
        current.release() // cannot bring refcount to zero
        // and create new current segment with old current as its parent
        current = new Segment(current)

        _r
      }

    // fill in the task for the new segment
    // submit the call by name argument to the executor service
    r.task = execServ.submit(new Callable[S] {
      override def call(): S = {
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

  def join[S](join: Revision[S]): S = {
    try {
      // wait for the revision to join to complete
      val res = join.task.get()

      // walk up its segment history
      var s = join.current
      while (s ne join.root) {
        for (v <- s.written) {
          // and merge everything that was written into this revision
          v.merge(this, join, s)
        }
        s = s.parent
      }

      res
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
  val currentRevision = new ThreadLocal[Revision[_]] {
    override def initialValue(): Revision[_] = {
      val s = new Segment
      new Revision[Nothing](s, s)
    }
  }

  // diagnostic method to expose the version number of the current segment version
  // for the current thread’s revision
  def currentVersion = currentRevision.get().current.version

  // fork an action from the current thread’s revision
  def fork[T](action: => T)(implicit execServ: ExecutorService): Revision[T] =
    currentRevision.get().fork(action)

  // join a revision into the current thread’s revision
  def join[T](join: Revision[T]): T =
    currentRevision.get().join(join)
}
