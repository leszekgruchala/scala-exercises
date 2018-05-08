package eu.gruchala.dll

trait MyDoubleLinkedList[A] {

  def prev: MyDoubleLinkedList[A]
  def next: MyDoubleLinkedList[A]
  def peek(): A
  def prepend(data: A): Unit
  def append(data: A): Unit
  def remove(): Unit
}

object MyDoubleLinkedList {

  def apply[A](data: A, more: A*): MyDoubleLinkedList[A] = {
    val dll = new MyDLinkedList(data)
    more.foreach(dll.append)
    dll
  }

  private[this] class MyDLinkedList[A](data: A) extends MyDoubleLinkedList[A] {
    var prev: MyDLinkedList[A] = _
    var next: MyDLinkedList[A] = _

    def peek(): A = this.data

    def prepend(data: A): Unit = {
      val newFirst = new MyDLinkedList(data)
      var oldFirst = this
      while (oldFirst.prev != null) {
        oldFirst = oldFirst.prev
      }
      newFirst.next = oldFirst
      oldFirst.prev = newFirst
    }

    def append(data: A): Unit = {
      val newNode = new MyDLinkedList(data)
      var last = this
      while (last.next != null) {
        last = last.next
      }
      newNode.prev = last
      last.next = newNode
    }

    def remove(): Unit = {
      val oldPrev = prev
      val oldNext = next
      if (oldPrev != null) {
        oldPrev.next = oldNext
      }
      if (oldNext != null) {
        oldNext.prev = oldPrev
      }
    }

    override def toString: String = data.toString
  }
}
