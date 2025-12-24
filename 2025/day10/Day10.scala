//> using dep "tools.aqua:z3-turnkey:4.14.1"
import scala.io.Source
import com.microsoft.z3._

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
    val ctx = new Context()
    val solver = ctx.mkOptimize()
    val buttons = lines
      .map(a => a.split(" ").init.tail)
      .map(b =>
        b.map(c => c.init.tail.split(",").map(x => x.toInt).toList).toList
      )

    val jolts = lines
      .map(a => a.split(" ").last.init.tail.split(","))
      .map(a => a.map(_.toInt))

    lines.zipWithIndex
      .map((l, ln) => {
        val buttonVars =
          buttons(ln).zipWithIndex
            .map((n, ix) =>
              ctx.mkIntConst(s"l${ln}b${ix.toString()}st-${n.mkString(",")}")
            )

        buttonVars.map(x => solver.Add(ctx.mkGe(x, ctx.mkInt(0))))

        getEqs(buttons(ln), buttonVars, jolts(ln), ctx)
          .foreach(x => solver.Add(x))
        val min = solver.MkMinimize(ctx.mkAdd(buttonVars*))
        val sat = solver.Check()
        if (sat == Status.SATISFIABLE) {
          val model: Model = solver.getModel
          min.toString.toInt
        } else 0
      })
      .sum
  }

  def getEqs(
      buttons: List[List[Int]],
      exprs: List[IntExpr],
      jolts: Array[Int],
      ctx: Context
  ) = {
    val out = jolts.zipWithIndex.map((jolt, joltix) =>
      ctx.mkEq(
        ctx.mkAdd(
          exprs.zipWithIndex
            .filter((ex, exidx) =>
              buttons.zipWithIndex
                .filter(b => b._1.contains(joltix))
                .map(_._2)
                .contains(exidx)
            )
            .map(_._1)*
        ),
        ctx.mkInt(jolt)
      )
    )
    out
  }

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()
    val input = Source.fromFile("2025/day10/input.txt").mkString

    // println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println(s"Run time: ${(System.nanoTime() - startTime) / 1000} microseconds")
  }
}
