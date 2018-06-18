package eu.gruchala.graphs

import org.scalatest.{Matchers, WordSpec}
import eu.gruchala.graphs.Graphs._

class GraphSpec extends WordSpec with Matchers {

  "Graph" should {

    "allow to check existence of a route between nodes" in {
      val directedGraph = IndexedSeq(
        Vertex("0", IndexedSeq(
            Vertex("1", IndexedSeq(Vertex("2", IndexedSeq(Vertex("0")))))
          )
        ),
        Vertex("3", IndexedSeq(Vertex("2")))
      )

      hasRoute(directedGraph, "0", "1") shouldBe true
      hasRoute(directedGraph, "1", "0") shouldBe false
      hasRoute(directedGraph, "2", "3") shouldBe false
      hasRoute(directedGraph, "3", "2") shouldBe true
      hasRoute(directedGraph, "1", "2") shouldBe true
      hasRoute(directedGraph, "2", "1") shouldBe false
      hasRoute(directedGraph, "2", "0") shouldBe true
      hasRoute(directedGraph, "3", "0") shouldBe false
    }

    "allow to travers and visit all vertexes" in {
      visitAll(graph).toIndexedSeq shouldBe IndexedSeq("A", "C", "B", "D")
    }

    "provide DepthFirstSearch (DFS) algorithm" in {
      import Graphs.Search.depthFirstSearch

      val (v, l) = depthFirstSearch(graph.head, "D")
      v shouldEqual Some(Vertex("D"))
      l should contain inOrderElementsOf IndexedSeq("A", "B", "C", "D")
      val (v2, l2) = depthFirstSearch(graph2, "5")
      v2 shouldEqual Some(Vertex("5"))
      l2 should contain inOrderElementsOf IndexedSeq("0", "1", "3", "2", "4", "5")
    }

    "provide BreadthFirstSearch (BFS) algorithm" in {
      import Graphs.Search.breadthFirstSearch

      val (v, l) = breadthFirstSearch(graph.head, "D")
      v shouldEqual Some(Vertex("D"))
      l should contain inOrderElementsOf IndexedSeq("A", "B", "D")

      val (v2, l2) = breadthFirstSearch(graph.head, "C")
      v2 shouldEqual Some(cVertex)
      l2 should contain inOrderElementsOf IndexedSeq("A", "B", "D", "C")

      val (v3, l3) = breadthFirstSearch(graph2, "100")
      v3 shouldEqual None
      l3 should contain inOrderElementsOf IndexedSeq("0", "1", "4", "5", "3", "2")
    }

    "allow cycle detection" when {
      "Graph (1,3),(3,5),(5,1) is provided" in {
        hasCycle(Seq((1,3),(3,5),(5,1))) shouldBe true
      }
      "Graph (1,1) is provided" in {
        hasCycle(Seq((1,1))) shouldBe true
      }
      "Graph (1,2),(2,1) is provided" in {
        hasCycle(Seq((1,2),(2,1))) shouldBe true
      }
      "Graph (2,1),(1,4),(4,1) is provided" in {
        hasCycle(Seq((2,1),(1,4),(4,1))) shouldBe true
      }
      "Graph (1,2)(2,3)(2,1)(3,1) is provided" in {
        hasCycle(Seq((1,2),(2,3),(2,1),(3,1))) shouldBe true
      }

      "Graph (1,3),(4,10),(5,2) is provided" in {
        hasCycle(Seq((1,3),(4,10),(5,2))) shouldBe false
      }
      "Graph (2,1),(1,3),(4,1) is provided" in {
        hasCycle(Seq((2,1),(1,3),(4,1))) shouldBe false
      }
      "Graph (1,3),(3,5),(5,2) is provided" in {
        hasCycle(Seq((1,3),(3,5),(5,2))) shouldBe false
      }
    }
  }
  private val cVertex = Vertex("C", IndexedSeq(Vertex("A", IndexedSeq())))
  val graph = IndexedSeq(
    Vertex("A", IndexedSeq(
        Vertex("B", IndexedSeq(cVertex)),
        Vertex("D", IndexedSeq())
      )
    )
  )
  private val vertex4 = Vertex("4")
  val graph2 = Vertex("0", IndexedSeq(
    Vertex("1", IndexedSeq(
      Vertex("3", IndexedSeq(Vertex("2"), vertex4)),
      vertex4
    )),
    vertex4,
    Vertex("5")
  ))

}

