package org.sandbox.adventofcode2018

import scala.io.Source

/**
  * @author roose
  */
object Day2p1 {

  def main(args: Array[String]): Unit = {
    val (totalTwos, totalThrees) =
      Source.fromFile("input/day2.txt").getLines()
        .foldLeft((0, 0)) { case ((twos, threes), cur) =>
          val counts =
            cur.foldLeft(Map.empty[Char, Int]) { (m, c) =>
              m.updated(c, m.getOrElse(c, 0) + 1)
            }
          val newTwos =
            if (counts.values.exists(_ == 2)) twos + 1
            else twos
          val newThrees =
            if (counts.values.exists(_ == 3)) threes + 1
            else threes
          (newTwos, newThrees)
        }
    println(totalTwos * totalThrees)
  }
}
