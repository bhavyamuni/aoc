import scala.io.Source

object Day06 {
  def part1(input: String): BigInt = {
    val lines = input.split("\n")
    val nums =
      lines.init.map(a =>
        a.trim.split("\\s+").map(b => if b != "" then BigInt(b) else BigInt(0))
      )
    val ops = lines.last.split("\\s+")

    val outs = ops.zipWithIndex.map((a, ix) => {
      a match {
        case "*" => nums.foldLeft(BigInt(1))((acc, state) => acc * state(ix))
        case "+" => nums.foldLeft(BigInt(0))((acc, state) => acc + state(ix))
      }
    })
    outs.sum
  }

  def part2(input: String): BigInt = {
    val lines = input.split("\n")
    val maxLineLength =
      lines.reduceLeft((a, b) => if a.length >= b.length then a else b).length
    val padded = lines.init.map(a => a + " " * (maxLineLength - a.length))
    val reversed = padded.map(_.reverse)
    val colNums: Array[String] =
      reversed.head.zipWithIndex.foldLeft(Array(""))((acc, curr) => {
        val (num, idx) = curr
        val foldedNum = reversed.foldLeft("")((a, b) => a + b(idx))
        acc :+ foldedNum
      })

    val cleanedUpNums: Array[Array[BigInt]] =
      colNums
        .foldLeft(Array(Array(BigInt(0))))((acc, curr) => {
          curr.trim() match {
            case ""               => acc :+ Array()
            case all if all != "" =>
              acc.init :+ (acc.last :+ BigInt(all.trim()))
          }
        })
        .tail
    val ops = lines.last.split("\\s+")

    val outs = ops.reverse.zipWithIndex.map((a, ix) => {
      a match {
        case "*" =>
          cleanedUpNums(ix).reduceLeft(_ * _)
        case "+" =>
          cleanedUpNums(ix).reduceLeft(_ + _)
      }
    })

    outs.sum
  }

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()
    val input = Source.fromFile("2025/day06/input.txt").mkString

    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println(s"Run time: ${(System.nanoTime() - startTime) / 1000} microseconds")
  }
}
