package eu.gruchala.dll

import eu.gruchala.BaseSpec

class MyDLinkedListSpec extends BaseSpec {

  "The custom DoubleLinkedLink implementation" should {
    "perform desired operation" when {
      "invoked append() method" in  {
        val dll = MyDoubleLinkedList("A")
        dll.append("B")
        dll.next.append("C")

        dll.next.next.peek() shouldEqual "C"
        dll.next.next.prev.peek() shouldEqual "B"
        dll.next.next.prev.prev.peek() shouldEqual "A"
      }

      "invoked prepend() method" in  {
        val dll = MyDoubleLinkedList("A")
        dll.prepend("B")
        dll.prev.prepend("C")

        dll.peek() shouldEqual "A"
        dll.prev.peek() shouldEqual "B"
        dll.prev.prev.peek() shouldEqual "C"

        dll.prev.prev.next.next.peek() shouldEqual "A"
        dll.prev.prev.next.peek() shouldEqual "B"
        dll.prev.next.prev.prev.peek() shouldEqual "C"
      }

      "invoked peek() method" in  {
        val dll = MyDoubleLinkedList("A")
        dll.prev shouldEqual null
        dll.next shouldEqual null
        dll.peek() shouldEqual "A"
        dll.append("B")
        dll.next.append("C")
        dll.next.next.peek() shouldEqual "C"

        dll.next.next.prev.peek() shouldEqual "B"
        dll.next.prev.peek() shouldEqual "A"
      }

      "invoked remove() method" in {
        val dll = MyDoubleLinkedList("A")
        dll.append("B")
        dll.append("C")
        dll.append("D")

        dll.next.next.remove() //remove C
        dll.next.next.peek() shouldEqual "D"
        dll.next.peek() shouldEqual "B"
        dll.peek() shouldEqual "A"

        dll.next.next.remove() //remove D
        dll.next.next shouldEqual null
        dll.peek() shouldEqual "A"
        dll.next.peek() shouldEqual "B"
      }

      "initialized with multiple values" in {
        val dll = MyDoubleLinkedList("A", "B", "C", "D")

        dll.next.next.remove() //remove C
        dll.next.next.peek() shouldEqual "D"
        dll.next.peek() shouldEqual "B"
        dll.peek() shouldEqual "A"

        dll.next.next.remove() //remove D
        dll.next.next shouldEqual null
        dll.peek() shouldEqual "A"
        dll.next.peek() shouldEqual "B"
      }

    }
  }

}
