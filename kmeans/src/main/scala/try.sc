
//val m = List((1, "a"), (2, "b"), (3, "a"), (4, "b"), (5, "a"))
//val g = m groupBy { _._2 }
//g mapValues { _ map {_._2}}

val m = List(("a", 1), ("b", 2), ("a", 3), ("b", 4), ("a", 5))
val g = m groupBy { _._1 }
val t = g mapValues {_ map {_._2}}
val means = List("a", "b", "c")
(for (m <- means) yield
  if (t contains m) (m, t(m)) else (m, Nil)).toMap

trait Iterator[T] {
  def hasNext: Boolean
  def next(): T
  def foldLeft[S](z: S)(f: (S, T) => S): S = {
    var result = z
    while (hasNext) result = f(result, next())
    result
  }
}

trait Traversable[T] {
  def foreach(f: T => Unit): Unit
  def newBuilder: Builder[T, Traversable[T]]
  def filter(p: T => Boolean): Traversable[T] = {
    val b = newBuilder
    foreach(t => if (p(t)) b += t)
    b.result
  }
}

import common._

import scala.collection.{GenSeq, GenTraversable}
import scala.collection.parallel.Combiner

val threshold = 5
def filter[T](s: GenTraversable[T], p: T => Boolean): GenTraversable[T] = {
  def aux(s: GenTraversable[T]): Combiner[T, GenTraversable[T]] = {
    if (s.size <= threshold) {
      val b = s.genericBuilder
      s.foreach((e: T) => if (p(e)) b += e) // what is problem?
      new Combiner[T,GenTraversable[A]](b)
    }
    else {
      val (l, r) = s.splitAt(s.size/2)
      val (lt, rt) =  (task { aux(l) }, task { aux(r) })
      val lc = lt.join(); val rc = rt.join()
      lc combine rc
    }
  }
  //
  aux(s).result()
}