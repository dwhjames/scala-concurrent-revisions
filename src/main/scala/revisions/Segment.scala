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
      for (v <- written) v.release(this)
      if (parent ne null) parent.release()
    }

  def collapse(main: Revision): Unit = {
    assert(main.current eq this)

    while ((parent ne main.root) && (parent.refCount.get() == 0)) {
      for (v <- parent.written) v.collapse(main, parent)
      parent = parent.parent
    }
  }
}

object Segment {
  private var versionCount = new AtomicInteger
}
