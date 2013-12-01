package revisions

import scala.collection.mutable

import java.util.concurrent.atomic.AtomicInteger


class Segment(
    private[revisions] var parent: Segment
) {
  private[revisions] val refCount = new AtomicInteger(1)
  private[revisions] val version: Int = Segment.versionCount.getAndIncrement()
  private[revisions] val written = mutable.ListBuffer.empty[Versioned]

  if (parent ne null) parent.refCount.getAndIncrement()

  def this() {
    this(null)
  }

  def release(): Unit =
    if (refCount.decrementAndGet() == 0) {
      // if ref count reaches zero
      // release the value for every versioned written in this segment
      for (v <- written) v.release(this)
      // and release the parent if it exists
      if (parent ne null) parent.release()
    }

  def collapse(main: Revision[_]): Unit = {
    // we should only be invoking this method on the current segment of the revision
    require(main.current eq this)

    // while parent is not the root, and parent is only referenced by this
    while ((parent ne main.root) && (parent.refCount.get() == 1)) {
      // collapse every versioned object written to in the parent
      for (v <- parent.written)
        v.collapse(main, parent)
      parent = parent.parent
    }
  }
}

object Segment {
  private var versionCount = new AtomicInteger
}
