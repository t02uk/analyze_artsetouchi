package com.twitter.t02uk.artsetouchi

import java.util.Calendar

case class DateTime(
    year: Int,
    month: Int,
    day: Int,
    hour: Int,
    min: Int,
    sec: Int) {
  
  val c = Calendar.getInstance()
  c.set(year, month, day, hour, min, sec)
  
  def weekOf = c.get(Calendar.DAY_OF_WEEK) match {
    case Calendar.SUNDAY => "Sun"
    case Calendar.MONDAY => "Mon"
    case Calendar.TUESDAY => "Tue"
    case Calendar.WEDNESDAY => "Wed"
    case Calendar.THURSDAY => "Thu"
    case Calendar.FRIDAY => "Fri"
    case Calendar.SATURDAY => "Sat"
  }
  
  def date = "%04d-%02d-%02d(%s)".format(year, month, day, weekOf)
  def time = "%02d-%02d-%02d".format(hour, min, sec)
  def hhmm = "%02d-%02d".format(hour, min)
  
  override def toString = "%04d-%02d-%02d-%02d-%02d-%02d".format(year, month, day, hour, min, sec)

}

object DateTime {
  implicit def string2int(string: String) = {
    if(string.isEmpty()) 0
    else string.toInt
  }

  def apply(dateTime: String): DateTime = {
    val re1 = """(\d{4})-(\d{2})-(\d{2})-(\d{2})-(\d{2})""".r
    val re2 = """(\d{4})-(\d{2})-(\d{2})-(\d{2})-(\d{2})-(\d{2})""".r
    dateTime match {
      case re1(year, month, day, hour, min) => DateTime(year, month, day, hour, min, 0)
      case re2(year, month, day, hour, min, sec) => DateTime(year, month, day, hour, min, sec)
    }
  }
  
  def apply(date: String, time: String): DateTime = apply(date.substring(0, 10) + "-" + time)
}