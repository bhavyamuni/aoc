import scala.io.Source

object DayXX {
  def part1(input: String): Int = {
    val lines = input.split("\n")
    0
  }

  def part2(input: String): Int = {
    val lines = input.split("\n")
    0
  }

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()
    val input = Source.fromFile("2025/dayXX/input.txt").mkString

    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println(s"Run time: ${(System.nanoTime() - startTime) / 1000} microseconds")
  }
}
