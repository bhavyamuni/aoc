import scala.io.Source

object Day10 {
  def part1(input: String): Int = {
    val lines = input.split("\n")
    val buttons = lines
      .map(a => a.split(" ").init.tail)
      .map(b =>
        b.map(c => c.init.tail.split(",").map(x => x.toInt).toSet).toSet
      )
    val finalStates = lines
      .map(a => a.split(" ").head.init.tail)
      .map(a => a.map(c => if c == '.' then 0 else 1).toList)

    finalStates.zip(buttons).map((a, b) => findPath(a, b)).sum
  }

  def findPath(finalState: List[Int], allButtons: Set[Set[Int]]) = {
    var memo: Map[(String, Int), Int] = Map()
    val finalStateSt = finalState.mkString
    val MAX_PRESSES = 20
    def dfs(
        state: List[Int],
        buttons: Set[Set[Int]],
        presses: Int
    ): Int = {
      state.mkString match {
        case a if a == finalStateSt           => presses
        case a if memo.contains((a, presses)) => memo((a, presses))
        case _ if presses > MAX_PRESSES       => Int.MaxValue
        case a                                => {
          val out = buttons
            .map(button =>
              dfs(
                outState(state, button),
                allButtons.filter(_ != button),
                presses + 1
              )
            )
            .min
          memo = memo + ((a, presses) -> out)
          out
        }
      }
    }

    val init = finalState.map(a => 0)
    val out = dfs(init, allButtons, 0)

    out
  }

  def outState(curr: List[Int], button: Set[Int]) = {
    curr.zipWithIndex.map((k, i) => if button.contains(i) then flip(k) else k)
  }

  def flip(i: Int) = if i == 1 then 0 else 1

  def part2(input: String): Int = {
    val lines = input.split("\n")

    0
  }

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()
    val input = Source.fromFile("2025/day10/input.txt").mkString

    // println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println(s"Run time: ${(System.nanoTime() - startTime) / 1000} microseconds")
  }
}
