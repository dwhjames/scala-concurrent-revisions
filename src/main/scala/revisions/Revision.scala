package revisions

import scala.annotation.elidable
import scala.concurrent.forkjoin.ForkJoinTask
import com.twitter.conversions.time._
import com.twitter.util.{Try, Await, Future, Promise}

import java.util.concurrent.Callable
import java.util.concurrent.atomic.AtomicInteger


class Revision[T](
    private[revisions] val root: Segment,
    private[revisions] var current: Segment
) {
  @volatile private var future: Future[T] = _
  @volatile private var joinSet: Set[Revision[_]] = _

  @elidable(elidable.ASSERTION) @inline
  private def updateJoinSetOnFork(r: Revision[_]): Unit = {
    r.joinSet = joinSet
    joinSet = joinSet + r
  }

  def fork[S](action: => S)(implicit exec: RevisionExecutor): Revision[S] = {
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

    updateJoinSetOnFork(r)

    val p = Promise[S]()

    // construct a Callable that will asynchronously execute
    // the call by name argument `action`
    val callable = new Callable[S] {
      override def call(): S = {
        val result = Try {
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
        p.update(result)
        result.get
      }
    }

    // fill in the task for the new revision
    r.future = p

    if (ForkJoinTask.inForkJoinPool()) {
      // if we are in a fork join thread
      // adapt the callable into a task and then fork it
      ForkJoinTask.adapt(callable).fork()
    } else {
      // else submit the callable to the fork join pool
      exec.forkJoinPool.submit(callable)
    }

    // finally return the new revision
    r
  }

  @elidable(elidable.ASSERTION) @inline
  private def unionJoinSets(r: Revision[_]): Unit = {
    joinSet = joinSet union r.joinSet
  }

  @elidable(elidable.ASSERTION) @inline
  private def removeRevisionFromJoinSet(r: Revision[_]): Unit = {
    joinSet = joinSet - r
  }

  def join[S](join: Revision[S]): S =
    Await.result(asyncJoin(join))

  def asyncJoin[S](join: Revision[S]): Future[S] = {
    /* fail if this revision is not in the set of valid revisions to join
     * this is the case if:
     *   1. the revision has already been joined
     *   2. the revision has been tunneled out isolation through shared memory
     */

    try {
      assume(joinSet contains join, "Invalid join on revision!")
    } catch {
      case t: AssertionError => return Future.exception(t)
    }

    join.future map { res =>
      // wait for the revision to join to complete

      unionJoinSets(join)

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

    } ensure {
      removeRevisionFromJoinSet(join)
      // release the segments in the joined revision
      join.current.release()
      // collapse the segments in this revision
      current.collapse(this)
    }
  }

  def abandon(join: Revision[_]): Unit = {
    assume(joinSet contains join, "Invalid join on revision!")

    try {
      Await.result(join.future)
      ()
    } finally {
      removeRevisionFromJoinSet(join)
      join.current.release()
      current.collapse(this)
    }
  }
}

object Revision {
  // the revision for the current thread
  val currentRevision = new ThreadLocal[Revision[_]] {
    override def initialValue(): Revision[_] = {
      val s = new Segment
      val r = new Revision[Nothing](s, s)
      r.joinSet = Set.empty
      r
    }
  }

  // diagnostic method to expose the version number of the current segment version
  // for the current thread’s revision
  // def currentVersion = currentRevision.get().current.version

  // fork an action from the current thread’s revision
  def fork[T](action: => T)(implicit execServ: RevisionExecutor): Revision[T] =
    currentRevision.get().fork(action)

  // join a revision into the current thread’s revision
  def join[T](join: Revision[T]): T =
    currentRevision.get().join(join)
}
