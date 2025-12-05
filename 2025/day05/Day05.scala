import scala.io.Source

object Day05 {
  def part1(input: String): Int = {
    val sections = input.split("\n\n")
    val ranges = sections.head.split("\n")
    val nums = sections.last.split("\n")

    nums
      .map(BigInt(_))
      .filter(a =>
        ranges
          .map(b =>
            (a >= (BigInt(b.split("-")(0))) && (a <= BigInt(b.split("-")(1))))
          )
          .exists(_ == true)
      )
      .length
  }

  def part2(input: String): Int = {
    val lines = input.split("\n")
    0
  }

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()
    val input = Source.fromFile("2025/day05/input.txt").mkString

    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println(s"Run time: ${(System.nanoTime() - startTime) / 1000} microseconds")
  }
}
