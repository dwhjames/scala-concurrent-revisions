package revisions

trait Versioned {
  def release(release: Segment): Unit
  def collapse(main: Revision, parent: Segment): Unit
  def merge(main: Revision, joinRev: Revision, join: Segment): Unit
}
