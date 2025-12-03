import scala.io.Source
import scala.math._

object Day01 {
  def part1(input: String): String = {
    val lines = input.split("\n")
    s"${getVectors(lines).scanLeft(50)((a, b) => (a + b) % 100).filter(_ == 0).length}"
  }

  def part2(input: String): String = {
    val lines = input.split("\n")
    val rs = getVectors(lines)
      .foldLeft((50, 0))({ case ((acc, out), curr) =>
        val newCurr = acc + curr
        val newVal = (((newCurr % 100) + 100) % 100)
        if (newCurr > 99 || acc == 0) {
          (newVal, out + (newCurr.abs) / 100)
        } else if (newCurr <= 0) {
          (newVal, out + 1 + (newCurr.abs) / 100)
        } else (newVal, out)
      })
      ._2
    s"$rs"
  }

  def getVectors(dirs: Array[String]): Array[Int] = {
    dirs.map(x => if (x.head == 'L') -x.tail.toInt else x.tail.toInt)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("2025/day01/input.txt").mkString

    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }
}
