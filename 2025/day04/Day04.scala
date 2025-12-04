import scala.io.Source

object Day04 {
  def part1(input: String): Int = {
    val lines = input.split("\n")
    val grid: Array[Array[Char]] = lines.map(a => a.toCharArray())
    val maxL = grid.length
    val maxW = grid.head.length
    val outs = (0 to maxL - 1)
      .map(a =>
        (0 to maxW - 1)
          .map(b =>
            if countAround((a, b), maxL, maxW, grid) < 4 && grid(a)(b) == '@'
            then 1
            else 0
          )
      )
      .flatten
      .sum
    outs
  }

  def countAround(
      curr: (Int, Int),
      maxX: Int,
      maxY: Int,
      matrix: Array[Array[Char]]
  ): Int = {
    val dirs: Array[(Int, Int)] = Array(
      (1, 0),
      (-1, 0),
      (0, 1),
      (0, -1),
      (1, 1),
      (1, -1),
      (-1, 1),
      (-1, -1)
    )
    dirs
      .map(a => (curr._1 + a._1, curr._2 + a._2))
      .filter((x, y) => x >= 0 && x < maxX && y >= 0 && y < maxY)
      .filter(a => matrix(a._1)(a._2) == '@')
      .length
  }

  def part2(input: String): Int = {
    val lines = input.split("\n")
    var grid: Array[Array[Char]] = lines.map(a => a.toCharArray())
    getTotalRemovals(grid)
  }

  def removeRollsFroMatrix(
      rolls: Array[(Int, Int)],
      matrix: Array[Array[Char]]
  ): Array[Array[Char]] = {
    matrix.zipWithIndex.map((a, i) =>
      a.zipWithIndex.map((b, j) => if rolls.contains((i, j)) then '.' else b)
    )
  }

  def getTotalRemovals(matrix: Array[Array[Char]]): Int = {
    val maxL = matrix.length
    val maxW = matrix.head.length
    val rollsToRemove = (0 to maxL - 1)
      .map(a =>
        (0 to maxW - 1)
          .map(b =>
            if countAround((a, b), maxL, maxW, matrix) < 4 && matrix(a)(
                b
              ) == '@'
            then ((a, b))
            else (-1, -1)
          )
      )
      .flatten
      .filter(a => a != (-1, -1))
      .toArray

    rollsToRemove.length match {
      case 0 => 0
      case _ =>
        rollsToRemove.length + getTotalRemovals(
          removeRollsFroMatrix(rollsToRemove, matrix)
        )
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("2025/day04/input.txt").mkString

    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }
}
