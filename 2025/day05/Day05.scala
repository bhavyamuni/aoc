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

  def part2(input: String): BigInt = {
    val sections = input.split("\n\n")
    val ranges = sections.head.split("\n")
    val sorted = ranges.sortBy(a => BigInt(a.split("-")(0)))

    val merged =
      sorted.scanLeft((BigInt(0), BigInt(0)))((state, curr) => {
        val currMin = BigInt(curr.split("-")(0))
        val currMax = BigInt(curr.split("-")(1))

        if (currMin < state._2) {
          (currMin.min(state._1), currMax.max(state._2))
        } else {
          (currMin, currMax)
        }
      })

    val ct = merged.foldRight((BigInt(0), BigInt(-1)))((curr, state) => {
      if (curr._1 == state._1) {
        state
      } else (curr._1, state._2 + (curr._2 - curr._1 + 1))
    })

    println(merged.mkString)
    println(ct._2)
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
