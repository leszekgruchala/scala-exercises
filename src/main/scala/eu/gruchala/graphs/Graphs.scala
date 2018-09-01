package eu.gruchala.graphs

import scala.annotation.tailrec

object Graphs {

  object Search {

    /**
      * Tail-recursive implementation of depth-first graph search algorithm.
      * Mostly used if we want to visit every node/vertex in the graph.
      * We start at the root (or selected node) and explore each branch completely,
      * before moving to another branch. We go deep before we go wide.
      */
    def depthFirstSearch(graph: Vertex, lookupNode: String): (Option[Vertex], IndexedSeq[String]) = {

      def moveOn(leftBranches: List[Vertex], visited: IndexedSeq[String]): (Option[Vertex], IndexedSeq[String]) = {
        leftBranches match {
          case Nil => None -> visited
          case h :: t => loop(h, t, visited)
        }
      }

      @tailrec
      def loop(node: Vertex, leftBranches: List[Vertex], visited: IndexedSeq[String]): (Option[Vertex], IndexedSeq[String]) = {
        if (visited.contains(node.name)) {
          moveOn(leftBranches, visited)
        } else {
          val allVisited = visited :+ node.name
          if (node.name == lookupNode) Some(node) -> allVisited
          else {
            node.edges.toList match {
              case Nil => moveOn(leftBranches, allVisited)
              case h :: t => loop(h, t ++ leftBranches, allVisited)
            }
          }
        }
      }

      loop(graph, Nil, IndexedSeq.empty)
    }

    /**
      * Tail-recursive implementation of breadth-first graph search algorithm.
      * Used to find shortest path between two nodes.
      * We start at the root (or selected node) and explore each neighbor,
      * before going to any child. We go wide before we go deep.
      */
    def breadthFirstSearch(graph: Vertex, lookupNode: String): (Option[Vertex], IndexedSeq[String]) = {

      def moveDeep(leftDeepNodes: List[Vertex], visited: IndexedSeq[String]): (Option[Vertex], IndexedSeq[String]) = {
        leftDeepNodes match {
          case Nil => None -> visited
          case h :: t => loop(h, h.edges.toList, t, visited)
        }
      }

      @tailrec
      def loop(node: Vertex, leftBranches: List[Vertex], leftDeepNodes: List[Vertex], visited: IndexedSeq[String]): (Option[Vertex], IndexedSeq[String]) = {
        if (visited.contains(node.name)) {
          leftBranches match {
            case Nil => moveDeep(leftDeepNodes, visited)
            case h :: t => loop(h, t, h :: leftDeepNodes, visited)
          }
        } else {
          val allVisited = visited :+ node.name
          if (node.name == lookupNode) Some(node) -> allVisited
          else {
            leftBranches match {
              case Nil =>
                node.edges.toList match {
                  case Nil => moveDeep(leftDeepNodes, allVisited)
                  case h :: t => loop(h, t ++ leftBranches, h :: leftDeepNodes, allVisited)
                }
              case h :: t => loop(h, t, h :: leftDeepNodes, allVisited)
            }
          }
        }
      }

      loop(graph, Nil, Nil, IndexedSeq.empty)
    }
  }

  def visitAll(graph: IndexedSeq[Vertex]): Set[String] = {

    def loop(current: Vertex, visited: Set[String]): Set[String] = {
      (for (next <- current.edges.toSet if !visited.contains(next.name))
        yield loop(next, visited + next.name)).flatten + current.name
    }

    graph.headOption.map { vertex =>
      loop(vertex, Set.empty)
    }.getOrElse(Set.empty)
  }

  def hasCycle(tuples: Seq[(Int, Int)]): Boolean = {

    val points = tuples.map { case (point, direction) =>
      Point(id = point, direction = direction)
    }

    def loop(startPoint: Point, graph: Seq[Point], visited: Seq[Point]): Boolean = {
      val nextDirection = startPoint.direction
      graph.find(_.id == nextDirection).exists { nextPoint =>
        if (visited.contains(nextPoint)) true
        else loop(nextPoint, graph, visited :+ nextPoint)
      }
    }

    points.headOption.exists { firstPoint =>
      loop(firstPoint, points, Seq(firstPoint))
    }
  }

  def hasRoute(graph: IndexedSeq[Vertex], a: String, b: String): Boolean = {

    @tailrec
    def loop(curr: Vertex, rest: List[Vertex]): Boolean = {
      if (curr.name == a) {
        curr.edges.exists(_.name == b)
      } else {
        rest match {
          case Nil => false
          case h :: t =>
            loop(h, h.edges.toList ::: t)
        }
      }
    }

    graph.exists(edge => loop(edge, edge.edges.toList))
  }
}
