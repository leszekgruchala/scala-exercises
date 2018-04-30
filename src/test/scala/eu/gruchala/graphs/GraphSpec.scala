package eu.gruchala.graphs

import org.scalatest.{Matchers, WordSpec}
import eu.gruchala.graphs.Graphs._

class GraphSpec extends WordSpec with Matchers {

  "Graph traversal" should {
    "travers all vertexes" in {
      graphTraversal(graph) shouldBe Set("A", "B", "C", "D")
    }
  }

  "Cycle detection" should {
    "return true" when {
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
    }

    "return false" when {
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


  val graph = Set(
    Vertex("A", Set(
      Vertex("B", Set(Vertex("C", Set(Vertex("A", Set()))))),
      Vertex("D", Set())
    )
    ))


}

