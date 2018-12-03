package org.sandbox.adventofcode2018

import scala.io.Source

/**
  * @author roose
  */
object Day3 {

  case class Claim(id: Int, x: Int, y: Int, w: Int, h: Int)

  class Fabric(w: Int, h: Int) {

    private val squares = new Array[Array[Int]](h)
    for (i <- squares.indices) {
      squares(i) = new Array[Int](w)
    }

    def applyClaim(claim: Claim): Unit = {
      for (y <- claim.y until (claim.y + claim.h)) {
        val row = squares(y)
        for (x <- claim.x until (claim.x + claim.w)) {
          row(x) += 1
        }
      }
    }

    def countOverlaps: Int =
      squares.map(_.count(_ > 1)).sum

    def isClaimOverlapped(claim: Claim): Boolean = {
      squares.slice(claim.y, claim.y + claim.h).exists(
        _.slice(claim.x, claim.x + claim.w).exists(_ > 1))
    }
  }

  def main(args: Array[String]): Unit = {
    val regex = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r("id", "x", "y", "w", "h")
    val claims = Source.fromFile("input/day3.txt").getLines().map { line =>
      val matched = regex.findFirstMatchIn(line).get
      Claim(
        matched.group("id").toInt,
        matched.group("x").toInt,
        matched.group("y").toInt,
        matched.group("w").toInt,
        matched.group("h").toInt)
    }.toList
    val (fabricWidth, fabricHeight) =
      claims.foldLeft((0, 0)) { case ((width, height), claim) =>
        (width max (claim.x + claim.w), height max (claim.y + claim.h))
      }
    val fabric = new Fabric(fabricWidth, fabricHeight)
    claims.foreach(fabric.applyClaim)
    println(fabric.countOverlaps)
    val unoverlapped = claims.find(!fabric.isClaimOverlapped(_)).map(_.id)
    println(unoverlapped)
  }
}
