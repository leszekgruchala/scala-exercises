//package eu.gruchala.proginscala
//
//trait Queue[+T] {
//  def head: T
//  def tail: List[T]
//  def enqueue[U >: T](x: U): Queue[T]
//}
//
//object Queue {
//  def apply[T](xs: T*) = new QueueImpl[T](xs.toList, Nil)
//
//  private class QueueImpl[+T] (
//    private val leading: List[T],
//    private val trailing: List[T]
//    ) extends Queue[T] {
//
//    private def mirror =
//      if (leading.isEmpty) {
//        new QueueImpl(trailing.reverse, Nil)
//      } else {
//        this
//      }
//
//    def head: T = mirror.leading.head
//
//    def tail: Queue[T] = {
//      val q = mirror
//      new QueueImpl(q.leading.tail, q.trailing)
//    }
//
//    def enqueue[U >: T](x: U) = {
//      new QueueImpl[U](leading, x :: trailing)
//    }
//  }
//}
