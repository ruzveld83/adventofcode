package org.sandbox.adventofcode2018

import scala.io.Source

/**
  * @author roose
  */
object Day2p2 {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input/day2.txt").getLines().toList

    def check(id1: String, id2: String): Option[String] =
      if (id1.length != id2.length) None
      else {
        val common = id1.zip(id2).filter { case (c1, c2) => c1 == c2 }.unzip._1.mkString("")
        if (common.length == id1.length - 1) Some(common)
        else None
      }

    def go2(id: String, ids: List[String]): Option[String] = ids match {
      case Nil => None
      case h :: t => check(id, h).orElse(go2(id, t))
    }

    def go1(ids: List[String]): Option[String] = ids match {
      case Nil => None
      case h :: t => go2(h, t).orElse(go1(t))
    }

    println(go1(lines))
  }
}
