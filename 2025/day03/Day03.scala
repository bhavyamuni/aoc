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

  def checkMax(possibilities: String): BigInt = {
    var out = Set[BigInt]()
    var st = Stack(0)

    for (ix <- 0 to (possibilities.length - 1)) {
      val i = possibilities(ix).asDigit
      while (
        st.length > 0 && st.top < i && (st.length + possibilities.length - ix) > 12
      ) {
        st.pop()
      }
      st.push(i)
      if (st.length == 12) {
        out += BigInt(st.map(_.toString).reduceRight((a, b) => b + a))
      }
    }
    out.max
  }

  def part2(input: String): BigInt = {
    val lines = input.split("\n")
    lines.foldLeft(BigInt(0))((a, b) => a + checkMax(b))
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("2025/day03/input.txt").mkString

    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }
}
