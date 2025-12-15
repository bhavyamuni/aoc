import scala.io.Source

case class PointXY(val x: BigInt, val y: BigInt)

object Day09 {
  def part1(input: String): BigInt = {
    val lines = input.split("\n")
    val points =
      lines.map(a => PointXY(BigInt(a.split(",")(0)), BigInt(a.split(",")(1))))
    var maxArea = 0;
    val pairs =
      for (a <- points; b <- points)
        yield ((a, b), calcArea(a, b))
    val maxA = pairs.maxBy(y => y._2)

    maxA._2
  }

  def calcArea(p1: PointXY, p2: PointXY): BigInt =
    (p1.x - p2.x + 1) * (p1.y - p2.y + 1)

  def part2(input: String): Int = {
    val lines = input.split("\n")
    0
  }

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()
    val input = Source.fromFile("2025/day09/input.txt").mkString

    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println(s"Run time: ${(System.nanoTime() - startTime) / 1000} microseconds")
  }
}
