package eu.gruchala.graphs

object Graphs {

  def graphTraversal(graph: Set[Vertex]): Set[String] = {

    def loop(current: Vertex, visited: Set[String]): Set[String] = {
      (for (next <- current.edges if !visited.contains(next.name))
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
