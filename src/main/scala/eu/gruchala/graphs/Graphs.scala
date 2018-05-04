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

      @tailrec //Cannot extract 'leftBranches' logic to preserve tail-recursion
      def loop(node: Vertex, leftBranches: List[Vertex], visited: IndexedSeq[String]): (Option[Vertex], IndexedSeq[String]) = {
        if (visited.contains(node.name)) {
          leftBranches match {
            case Nil => None -> visited
            case h :: t => loop(h, t, visited)
          }
        } else {
          val allVisited = visited :+ node.name
          if (node.name == lookupNode) Some(node) -> allVisited
          else {
            node.edges.toList match {
              case Nil =>
                leftBranches match {
                  case Nil => None -> allVisited
                  case h :: t => loop(h, t, allVisited)
                }

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
      @tailrec //Cannot extract 'leftBranches' and 'leftDeepNodes' logic to preserve tail-recursion
      def loop(node: Vertex, leftBranches: List[Vertex], leftDeepNodes: List[Vertex], visited: IndexedSeq[String]): (Option[Vertex], IndexedSeq[String]) = {
        if (visited.contains(node.name)) {
          leftBranches match {
            case Nil =>
              leftDeepNodes match {
                case Nil => None -> visited
                case h :: t => loop(h, h.edges.toList, t, visited)
              }
            case h :: t => loop(h, t, h :: leftDeepNodes, visited)
          }
        } else {
          val allVisited = visited :+ node.name
          if (node.name == lookupNode) Some(node) -> allVisited
          else {
            if (leftBranches.nonEmpty) {
              leftBranches match {
                case Nil =>
                  leftDeepNodes match {
                    case Nil => None -> allVisited
                    case h :: t => loop(h, t, h :: leftDeepNodes, allVisited)
                  }
                case h :: t => loop(h, t, h :: leftDeepNodes, allVisited)
              }
            } else {
              node.edges.toList match {
                case Nil =>
                  leftDeepNodes match {
                    case Nil => None -> allVisited
                    case h :: t => loop(h, h.edges.toList, t, allVisited)
                  }
                case h :: t => loop(h, t ++ leftBranches, h :: leftDeepNodes, allVisited)
              }
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


}
