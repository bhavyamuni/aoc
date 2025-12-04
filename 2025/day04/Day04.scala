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

  // def checkNeighbors(curr: (Int, Int), matrix: Array[Array[Char]]): Int = {
  //   val maxL = matrix.length
  //   val maxW = matrix.head.length
  //   curr match {
  //     case (a, b)
  //         if a < 0 || b < 0 || a >= matrix.length || b >= matrix.head.length =>
  //       0
  //     case (a, b)
  //         if countAround(curr, maxL, maxW)
  //           .filter(a => matrix(a._1)(a._2) == '@')
  //           .length >= 8 =>
  //       1 + checkNeighbors((curr._1 + 1, curr._2), matrix) + checkNeighbors(
  //         (curr._1, curr._2 + 1),
  //         matrix
  //       )
  //     case _ => 0
  //   }
  // }

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
    0
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("2025/day04/input.txt").mkString

    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }
}
