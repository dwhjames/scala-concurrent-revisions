package revisions

trait ThreeWayMerge[T] extends AbstractVersioned[T] {

  protected def mergeValue(root: T, main: T, join: T): T

  override protected def computeMerge(main: Revision[_], joinRev: Revision[_], join: Segment): T = {
    val idx = getIndex(main.current)
    if (idx < 0)
      versions(join.version)
    else
      mergeValue(get(joinRev.root), versions(idx), versions(join.version))
  }

}
