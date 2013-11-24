package revisions

trait Versioned {
  def release(release: Segment): Unit
  def collapse(main: Revision[_], parent: Segment): Unit
  def merge(main: Revision[_], joinRev: Revision[_], join: Segment): Unit
}
