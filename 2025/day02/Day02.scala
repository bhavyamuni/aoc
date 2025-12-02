import scala.io.Source

object Day02 {
  def part1(input: String): BigInt = {
    val lines = input.split("\n")
    val ranges = lines.head.split(",")
    val out =
      ranges.foldLeft(BigInt(0))((acc, curr) =>
        acc + iterateAndCheck(
          getMinAndMax(curr)._1,
          getMinAndMax(curr)._2,
          isRepeated
        ).sum
      )
    out
  }

  def getMinAndMax(input: String): (BigInt, BigInt) =
    (BigInt(input.split("-")(0)), BigInt(input.split("-")(1)))

  def isRepeated(input: String): Boolean = {
    input match
      case ""                                                 => true
      case _ if input.length % 2 == 1                         => false
      case _ if input.length == 2 && input.head == input.last => true
      case _                                                  =>
        isRepeated(
          input.tail.take(input.tail.length / 2) +
            input.tail.drop(input.tail.length / 2 + 1)
        ) && input.head == input(input.length / 2)
  }

  def iterateAndCheck(
      min: BigInt,
      max: BigInt,
      repeatFunc: Function[String, Boolean]
  ): Seq[BigInt] = {
    (min to max).filter((a) => repeatFunc(a.toString))
  }

  def part2(input: String): BigInt = {
    val lines = input.split("\n")
    val ranges = lines.head.split(",")
    val out =
      ranges.foldLeft(BigInt(0))((acc, curr) =>
        acc + iterateAndCheck(
          getMinAndMax(curr)._1,
          getMinAndMax(curr)._2,
          isRepeatedElegant
        ).sum
      )
    out
  }

  def isRepeatedElegant(input: String): Boolean = {
    (input + input).init.tail.contains(input)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("2025/day02/input.txt").mkString

    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }
}
