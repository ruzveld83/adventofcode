package org.sandbox.adventofcode2018

import scala.collection.immutable.Stack
import scala.io.Source

/**
  * @author roose
  */
object Day5p1 {

  def main(args: Array[String]): Unit = {
    val result =
      Source
        .fromFile("input/day5.txt")
        .getLines()
        .flatMap(_.toCharArray)
        .foldLeft(Stack.empty[Char]) { (stack, c) =>
          if (stack.isEmpty) {
            stack.push(c)
          } else {
            val (poppedChar, poppedStack) = stack.pop2
            if (c.toLower == poppedChar.toLower && c.isLower != poppedChar.isLower) {
              poppedStack
            } else {
              stack.push(c)
            }
          }
        }
        .size
    println(result)
  }
}
