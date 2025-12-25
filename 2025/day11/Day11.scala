import scala.io.Source
import scala.collection.immutable.Queue
object Day11 {
  def part1(input: String): Int = {
    val lines = input.split("\n")
    val adj =
      lines
        .map(l => l.split(": ").head -> l.split(": ").last.split(" ").toSet)
        .toMap
    visitPaths(adj)
  }

  def visitPaths(adjList: Map[String, Set[String]]) = {
    var q = Queue("you")
    var level = 0
    while (q.nonEmpty) {
      val curr = q.dequeue._1
      q = q.dequeue._2
      curr match {
        case "out" =>
          level += 1
        case x => q = q.appendedAll(adjList(x))
      }
    }
    level
  }

  def part2(input: String): Int = {
    val lines = input.split("\n")
    0
  }

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()
    val input = Source.fromFile("2025/day11/input.txt").mkString

    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println(s"Run time: ${(System.nanoTime() - startTime) / 1000} microseconds")
  }
}
