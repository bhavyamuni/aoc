import scala.io.Source
import scala.math._

object Day01 {
  def part1(input: String): String = {
    val lines = input.split("\n")
    val maxLen = lines.length
    s"${getVectors(lines).scanLeft(50)((a, b) => if (a+b > 0) then (a+b)%100 else (a+b+100)%100).filter(_ == 0).length}"
  }

  def part2(input: String): String = {
    val lines = input.split("\n")
    s"${getVectors(lines).scanLeft(50)((a, b) =>
      if (a+b > 0) then (a+b)%100
      else (a+b+100)%100).filter(_ == 0).length}"
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
