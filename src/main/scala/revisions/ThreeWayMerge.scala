package revisions

trait ThreeWayMerge[T] extends AbstractVersioned[T] {

  protected def mergeValue(root: T, main: T, join: T): T

  override protected def computeMerge(main: Revision[_], joinRev: Revision[_], join: Segment): T = {
    val mainIdx = getIndex(main.current)
    if (mainIdx < 0)
      versions(join.version)
    else {
      // mainIdx is non-negative, so rootIdx must be as well
      val rootIdx = getIndex(joinRev.root)
      if (rootIdx == mainIdx) // if main has not written since the root
        versions(join.version)
      else
        mergeValue(versions(rootIdx), versions(mainIdx), versions(join.version))
    }
  }

}
