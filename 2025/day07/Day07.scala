import scala.io.Source
import scala.collection.mutable.Stack
import scala.collection.immutable.Queue
import scala.annotation.tailrec

object Day07 {
  def part1(input: String): Int = {
    val lines = input.split("\n")
    val matrix = lines.map(_.toCharArray())
    val startI =
      matrix.zipWithIndex
        .filter((a, i) => if a.indexOf('S') >= 0 then true else false)
        .head
        ._2
    val startJ: Int =
      matrix.zipWithIndex
        .filter((a, i) => if a.indexOf('S') >= 0 then true else false)
        .head
        ._1
        .indexOf('S')

    splitterBFS((startI + 1, startJ), matrix)
    // splitter((startI + 1, startJ), matrix, Set())
  }

  // BFS
  def splitterBFS(
      curr: (Int, Int),
      matrix: Array[Array[Char]]
  ): Int = {
    var q = Queue[(Int, Int)](curr)
    var res = 0
    var visited = Set[(Int, Int)]()
    while (!q.isEmpty) {
      // println(q.head)
      q.dequeue match {
        case ((a, b), newQ)
            if a >= matrix.length || b < 0 || b >= matrix.head.length || visited
              .contains((a, b)) =>
          q = newQ
        case ((a, b), newQ) if matrix(a)(b) == '^' =>
          q = newQ.enqueue((a, b + 1)).enqueue((a, b - 1))
          visited += ((a, b))
          res += 1
        case ((a, b), newQ) =>
          visited += ((a, b))
          q = newQ.enqueue((a + 1, b))
      }
    }
    res
  }

  def part2(input: String): BigInt = {
    val lines = input.split("\n")
    val matrix = lines.map(_.toCharArray())
    val startI =
      matrix.zipWithIndex
        .filter((a, i) => if a.indexOf('S') >= 0 then true else false)
        .head
        ._2
    val startJ: Int =
      matrix.zipWithIndex
        .filter((a, i) => if a.indexOf('S') >= 0 then true else false)
        .head
        ._1
        .indexOf('S')

    helper((startI + 1, startJ), matrix)
  }

  def helper(curr: (Int, Int), matrix: Array[Array[Char]]) = {
    var memo = Map[(Int, Int), BigInt]()

    def splitter(curr: (Int, Int), matrix: Array[Array[Char]]): BigInt = {
      curr match {
        case (a, b) if memo.contains((a, b)) => memo((a, b))
        case (a, b) if a > matrix.length - 1 => {
          memo = memo.updated((a, b), 0)
          0
        }
        case (a, b) if b <= 0 || b >= matrix.head.length => {
          memo = memo.updated((a, b), 1)
          1
        }
        // case (a, b) if visited.contains((a, b))          =>
        //   splitter((a + 1, b), matrix, visited)
        case (a, b) if matrix(a)(b) == '^' => (
          {
            val tot = splitter((a + 1, b + 1), matrix) + splitter(
              (a + 1, b - 1),
              matrix
            ) + 1

            memo = memo.updated((a, b), tot)
            tot
          }
        )
        case (a, b) =>
          splitter((a + 1, b), matrix)
      }
    }
    splitter(curr, matrix)
  }

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()
    val input = Source.fromFile("2025/day07/input.txt").mkString

    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println(s"Run time: ${(System.nanoTime() - startTime) / 1000} microseconds")
  }
}
