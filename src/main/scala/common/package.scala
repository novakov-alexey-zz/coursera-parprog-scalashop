
import java.util.concurrent._
import scala.util.DynamicVariable

package object common {

  val forkJoinPool = new ForkJoinPool

  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]
    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    scheduler.value.parallel(taskA, taskB)
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task { taskA }
    val tb = task { taskB }
    val tc = task { taskC }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }

  /**
    * Returns array of ranges. Each pair of array repsent a page range similar to content in a book
    * @param whole a number to be divided
    * @param numTasks required number of tasks
    * @return
    */
  def getRanges(whole: Int, numTasks: Int): Array[Int] = {
    val parts = splitIntoParts(whole, numTasks)
    parts.foldLeft(Array(0))((arr, e) => arr :+ parts.take(arr.length - 1).sum + e)
  }

  /**
    * Split a number into equal parts
    *
    * @param whole - a number to be divided into equal parts
    * @param parts - number of required parts
    * @return array of parts
    */
  def splitIntoParts(whole: Int, parts: Int): Array[Int] = {
    val arr = new Array[Int](parts)
    var w = whole

    for (i <- arr.indices) {
      arr(i) = (w + parts - i - 1) / (parts - i)
      w -= arr(i)
    }

    arr
  }
}
