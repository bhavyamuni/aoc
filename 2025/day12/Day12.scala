import scala.io.Source

object Day12 {
  def part1(input: String): BigInt = {
    val shapes = input.split("\n\n").init
    val gridsStr = input.split("\n\n").last.split("\n")
    val grids = gridsStr.map(line =>
      (
        (
          BigInt(line.split(": ").head.split("x").head),
          BigInt(line.split(": ").head.split("x").last)
        ),
        line.split(": ").last.split(" ").map(BigInt(_)).sum
      )
    )

    val fitting = grids.filter((x) => x._1._1 * x._1._2 >= x._2 * 9)
    fitting.length
  }

  def part2(input: String): Int = {
    val lines = input.split("\n")
    0
  }

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()
    val input = Source.fromFile("2025/day12/input.txt").mkString

    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println(s"Run time: ${(System.nanoTime() - startTime) / 1000} microseconds")
  }
}
