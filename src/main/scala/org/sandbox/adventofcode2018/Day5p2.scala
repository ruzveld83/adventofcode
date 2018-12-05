package org.sandbox.adventofcode2018

import scala.collection.immutable.Stack
import scala.io.Source

/**
  * @author roose
  */
object Day5p2 {

  def main(args: Array[String]): Unit = {
    val chars =
      Source
        .fromFile("input/day5.txt")
        .getLines()
        .flatMap(_.toCharArray)
        .toList
    val uniqueChars = chars.iterator.map(_.toLower).toSet
    val result =
      uniqueChars.map { charToExclude =>
        chars
          .filter(_.toLower != charToExclude)
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
      }.min
    println(result)
  }
}
