import scala.io.Source

object Day03 {
  def part1(input: String): Int = {
    val lines = input.split("\n")
    lines.foldLeft(0)(_ + findMaxVals(_))
  }

  def findMaxVals(input: String): Int = {
    input
      .split("")
      .map(a => a.toInt)
      .foldLeft((0, 0))((state, curr) => {
        val (currMax, out) = state
        val newVal = currMax * 10 + curr
        (currMax.max(curr), out.max(newVal))
      })
      ._2
  }

  def part2(input: String): Int = {
    val lines = input.split("\n")
    0
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("2025/day03/input.txt").mkString

    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }
}
