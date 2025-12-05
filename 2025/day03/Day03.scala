import scala.io.Source
import scala.collection.mutable.Stack

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

  def checkMaxRecurs(
      possibilities: String,
      st: Stack[Int],
      acc: BigInt
  ): BigInt = {
    st.length match {
      case 12 =>
        acc.max(BigInt(st.map(_.toString).reduceRight((a, b) => b + a)))
      case _ if possibilities.length <= 0 => BigInt(0)
      case _
          if st.length > 0 && st.top < possibilities.head.asDigit && (st.length + possibilities.length) > 12 =>
        checkMaxRecurs(
          possibilities,
          st.tail,
          acc
        )
      case _ =>
        checkMaxRecurs(
          possibilities.tail,
          st.push(possibilities.head.asDigit),
          acc
        )
    }
  }

  // imperative
  def checkMax(possibilities: String): BigInt = {
    var st = Stack(0)
    possibilities.zipWithIndex
      .foldLeft(BigInt(0))((acc, state) => {
        val (x, idx) = state
        while (
          st.length > 0 && st.top < x.asDigit && (st.length + possibilities.length - idx) > 12
        ) {
          st.pop()
        }
        st.push(x.asDigit)
        if st.length == 12 then
          acc.max(BigInt(st.map(_.toString).reduceRight((a, b) => b + a)))
        else acc
      })
  }

  def part2(input: String): BigInt = {
    val lines = input.split("\n")
    lines.foldLeft(BigInt(0))((a, b) => a + checkMaxRecurs(b, Stack(0), 0))
  }

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()
    val input = Source.fromFile("2025/day03/input.txt").mkString

    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println(s"Run time: ${(System.nanoTime() - startTime) / 1000} microseconds")
  }
}
