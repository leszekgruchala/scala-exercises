package eu.gruchala.graphs

case class Vertex(name: String, edges: IndexedSeq[Vertex] = IndexedSeq.empty)
