package org.sandbox.adventofcode2018

import scala.io.Source

/**
  * @author roose
  */
object Day4 {

  sealed trait Message
  case object FallAsleep extends Message
  case object WakeUp extends Message
  case class BeginShift(id: Int) extends Message

  case class Record(year: Int, month: Int, day: Int, hour: Int, minute: Int, msg: Message)

  case class SleepInterval(id: Int, from: Option[Int] = None, to: Option[Int] = None)

  def main(args: Array[String]): Unit = {
    val regex = """\[(\d{4})-(\d\d)-(\d\d) (\d\d):(\d\d)\] (.+)""".r("y", "M", "d", "h", "m", "msg")
    val beginShiftRegex = """Guard #(\d+)""".r("id")
    val records =
      Source.fromFile("input/day4.txt").getLines().map { line =>
        val matched = regex.findFirstMatchIn(line).get
        val message = matched.group("msg") match {
          case "falls asleep" =>
            FallAsleep
          case "wakes up" =>
            WakeUp
          case beginShiftStr =>
            val id = beginShiftRegex.findFirstMatchIn(beginShiftStr).get.group("id").toInt
            BeginShift(id)
        }
        Record(
          matched.group("y").toInt,
          matched.group("M").toInt,
          matched.group("d").toInt,
          matched.group("h").toInt,
          matched.group("m").toInt,
          message)
      }.toArray
    val sorted = records.sortBy(r => (r.year, r.month, r.day, r.hour, r.minute))
    val sleepIntervals = sorted.foldLeft(Vector.empty[SleepInterval]) { (intervals, cur) =>
      cur.msg match {
        case BeginShift(id) =>
          intervals :+ SleepInterval(id)
        case FallAsleep =>
          val last = intervals.last
          last.from match {
            case None =>
              val si = last.copy(from = Some(cur.minute))
              intervals.updated(intervals.size - 1, si)
            case _ =>
              val si = SleepInterval(last.id, Some(cur.minute))
              intervals :+ si
          }
          case WakeUp =>
          val si = intervals.last.copy(to = Some(cur.minute - 1))
          intervals.updated(intervals.size - 1, si)
      }
    }.filter(_.from.isDefined)
    val idToMinutes = sleepIntervals.foldLeft(Map.empty[Int, Array[Int]]) { (idToMinutes, interval) =>
      val minutes = idToMinutes.getOrElse(interval.id, new Array[Int](60))
      val from = interval.from.get
      val to = interval.to.getOrElse(60)
      for (i <- from to to) {
        minutes(i) += 1
      }
      idToMinutes.updated(interval.id, minutes)
    }
    val sleepy = idToMinutes.maxBy(_._2.sum)._1
    val topMinute = idToMinutes(sleepy).zipWithIndex.maxBy(_._1)._2
    println(sleepy * topMinute)
    val (sleepy2, _, topMinute2) = idToMinutes.map { case (id, minutes) =>
      val (count, minute) = minutes.zipWithIndex.maxBy(_._1)
      (id, count, minute)
    }.maxBy(_._2)
    println(sleepy2 * topMinute2)
  }
}
