package revisions

import java.util.concurrent.atomic.AtomicInteger


class Segment {
  private[revisions] var refCount = new AtomicInteger(1)

  var parent: Segment = null
  val version: Int = Segment.versionCount.getAndIncrement()
  var written: Seq[Versioned] = List()

  def this(parent: Segment) {
    this()
    this.parent = parent
    if (parent ne null) parent.refCount.getAndIncrement()
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
