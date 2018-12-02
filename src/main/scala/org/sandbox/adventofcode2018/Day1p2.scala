package org.sandbox.adventofcode2018

import scala.collection.mutable
import scala.io.Source

/**
  * @author roose
  */
object Day1p2 {

  def main(args: Array[String]): Unit = {
    val set = mutable.HashSet.empty[Long]
    def fileIter: Iterator[Long] = Source.fromFile("input/day1.txt").getLines().map(_.toLong)
    val result = Iterator.continually(fileIter).flatten
        .scanLeft(0L)(_ + _)
        .find(!set.add(_))
    println(result)
  }
}
