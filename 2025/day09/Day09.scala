import scala.io.Source

case class PointXY(val x: BigInt, val y: BigInt)

object Day09 {
  def part1(input: String): BigInt = {
    val lines = input.split("\n")
    val points =
      lines.map(a => PointXY(BigInt(a.split(",")(0)), BigInt(a.split(",")(1))))
    var maxArea = 0;
    val pairs =
      for (a <- points; b <- points)
        yield ((a, b), calcArea(a, b))
    val maxA = pairs.maxBy(y => y._2)

    maxA._2
  }

  def calcArea(p1: PointXY, p2: PointXY): BigInt = {
    val minX = if p1.x <= p2.x then p1.x else p2.x
    val maxX = if p1.x > p2.x then p1.x else p2.x
    val minY = if p1.y <= p2.y then p1.y else p2.y
    val maxY = if p1.y > p2.y then p1.y else p2.y
    (maxX - minX + 1) * (maxY - minY + 1)
  }

  def part2(input: String): BigInt = {
    val lines = input.split("\n")

    val points =
      lines.map(a => PointXY(BigInt(a.split(",")(0)), BigInt(a.split(",")(1))))

    val border =
      (points)
        .scanLeft(points.last)((state, curr) => {
          (curr.x - state.x, curr.y - state.y) match {
            case (a, 0) if a > 0 => PointXY(curr.x, curr.y - 1)
            case (a, 0) if a < 0 => PointXY(curr.x, curr.y + 1)
            case (0, b) if b > 0 => PointXY(curr.x + 1, curr.y)
            case (0, b) if b < 0 => PointXY(curr.x - 1, curr.y)
            case (_, _)          => curr
          }
        })
        .tail
    val correctBorder = (border.last +: border).zipWithIndex
      .map((v, ix) => {
        if ix >= points.length then v
        else
          val nxt = points(ix)
          (v.x - nxt.x, v.y - nxt.y) match {
            case (0, b) if b < 0 => PointXY(v.x + 1, v.y)
            case (0, b) if b > 0 => PointXY(v.x - 1, v.y)
            case (a, 0) if a > 0 => PointXY(v.x, v.y + 1)
            case (a, 0) if a < 0 => PointXY(v.x, v.y - 1)
            case (_, _)          => v
          }
      })
      .init

    val fullBorder = correctBorder.foldLeft(
      (Set[PointXY](), correctBorder.last)
    )((acc, curr) => {
      val (s, prev) = acc
      val decY = if curr.y > prev.y then 1 else -1
      val decX = if curr.x > prev.x then 1 else -1
      val ps =
        for (i <- (prev.x to curr.x by decX); j <- (prev.y to curr.y by decY))
          yield PointXY(i, j)
      (s ++ ps.toSet, curr)
    })
    val borderSet = fullBorder._1

    val pairs =
      for (
        a <- points; b <- points;
        if (a != b)
      )
        yield ((a, b), calcArea(a, b))

    val maxA =
      pairs
        .sortBy(y => -y._2)
        .find(x => isValid(x._1._1, x._1._2, borderSet))

    maxA.head._2
  }

  def isValid(p1: PointXY, p2: PointXY, border: Set[PointXY]) = {
    val minX = if p1.x <= p2.x then p1.x else p2.x
    val maxX = if p1.x > p2.x then p1.x else p2.x
    val minY = if p1.y <= p2.y then p1.y else p2.y
    val maxY = if p1.y > p2.y then p1.y else p2.y

    val anyPtInside =
      border.find(x =>
        (x.x >= minX) && (x.x <= maxX) && (x.y >= minY) && (x.y <= maxY)
      )
    !anyPtInside.isDefined
  }

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()
    val input = Source.fromFile("2025/day09/input.txt").mkString

    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println(s"Run time: ${(System.nanoTime() - startTime) / 1000} microseconds")
  }
}
