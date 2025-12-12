import scala.io.Source
import scala.math._
import scala.collection.mutable.PriorityQueue

case class Point(
    val x: BigInt,
    val y: BigInt,
    val z: BigInt
)

object Day08 {
  def part1(input: String): BigInt = {
    val lines = input.split("\n").sortBy(a => a.split(",")(0).toInt)
    val points =
      lines.map(a =>
        Point(
          BigInt(a.split(",")(0)),
          BigInt(a.split(",")(1)),
          BigInt(a.split(",")(2))
        )
      )

    val distances =
      for (
        a <- points.zipWithIndex; b <- points.slice(a._2 + 1, points.length + 1)
        if a._1 != b
      )
        yield ((a._1, b), distance(a._1, b))

    val pq =
      PriorityQueue[((Point, Point), BigInt)]()(using Ordering.by(_._2 * -1))
    pq.addAll(distances)

    val initMp: Map[Point, Set[Point]] = points.map(a => a -> Set.empty).toMap
    val juncs = makeIslands(10, pq, initMp)
    // println(juncs.sortBy(a => -a.size).mkString("\n"))
    println(juncs.mkString("\n"))
    // calculateTop3(juncs)
    helper(juncs)
  }

  def calculateTop3(mp: Array[Set[Point]]): Int =
    mp.map(a => a.size).sortBy(a => a).slice(0, 3).reduceLeft(_ * _)

  def makeIslands(
      connections: Int,
      pq: PriorityQueue[((Point, Point), BigInt)],
      mp: Map[Point, Set[Point]]
  ): Map[Point, Set[Point]] = {
    val lowest = pq.dequeue()
    (lowest, connections) match {
      case (_, 0) => mp.filter((a, b) => !b.isEmpty)
      case (((p1, p2), dist), _)
          if mp(p1).contains(p2) || mp(p2).contains(p1) =>
        makeIslands(
          connections,
          pq,
          mp.updated(p1, mp(p1) + p2)
            .updated(p2, mp(p2) + p1)
        )
      case (((p1, p2), dist), _) =>
        makeIslands(
          connections - 1,
          pq,
          mp.updated(p1, mp(p1) + p2)
            .updated(p2, mp(p2) + p1)
        )
    }
  }

  def helper(
      al: Map[Point, Set[Point]]
  ): BigInt = {
    var visited: Set[Point] = Set.empty
    def dfs(curr: Point): BigInt = {
      curr match {
        case a if visited.contains(a) => BigInt(0)
        case a                        => {
          visited += a
          1 + al(a).map(dfs(_)).sum
        }
      }
    }

    val allVals = al
      .map((k, v) => {
        dfs(k)
      })
      .toList
      .sortBy(a => -a)

    allVals.take(3).reduceLeft(_ * _)
  }

  def distance(p1: Point, p2: Point): BigInt = {
    ((p1.x - p2.x) * (p1.x - p2.x)) + ((p1.y - p2.y) * (p1.y - p2.y)) + ((p1.z - p2.z) * (p1.z - p2.z))
  }

  def part2(input: String): Int = {
    val lines = input.split("\n")
    0
  }

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()
    val input = Source.fromFile("2025/day08/input.txt").mkString

    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println(s"Run time: ${(System.nanoTime() - startTime) / 1000} microseconds")
  }
}
