package revisions

import scala.concurrent.forkjoin.ForkJoinPool

trait RevisionExecutor {
  def forkJoinPool: ForkJoinPool
}

object RevisionExecutor {

  def global: RevisionExecutor = Implicits.global

  object Implicits {
    implicit lazy val global: RevisionExecutor = new RevisionExecutor {
      override val forkJoinPool = new ForkJoinPool()
    }
  }
}
