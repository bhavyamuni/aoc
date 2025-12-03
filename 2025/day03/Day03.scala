import scala.io.Source

object Day03 {
  def part1(input: String): Int = {
    val lines = input.split("\n")
    lines.foldLeft(0)((acc, a) => acc + findMaxVals(a))
  }

  def findMaxVals(input: String): Int = {
    input
      .split("")
      .foldLeft(("0", 0))((state, curr) => {
        val (currMax, out) = state
        val newVal = currMax + curr
        (((currMax.toInt).max(curr.toInt).toString), out.max(newVal.toInt))
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
