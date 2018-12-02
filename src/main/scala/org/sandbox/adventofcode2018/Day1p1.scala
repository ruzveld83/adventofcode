package org.sandbox.adventofcode2018

import scala.io.Source

/**
  * @author roose
  */
object Day1p1 {

  def main(args: Array[String]): Unit = {
    val result = Source.fromFile("input/day1.txt").getLines().map(_.toLong).sum
    println(result)
  }
}
