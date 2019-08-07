package com.processChain

import com.github.tototoshi.csv.CSVReader

import scala.util.control.NonFatal

object Program {
  def main(args: Array[String]): Unit = {
    // operation pluck(11) -> avg -> ceil

    val path = args.headOption.getOrElse("0.csv")
    val reader = CSVReader.open(path)
    try {
      val csvIterator = new Iterable[Seq[String]]{ def iterator = reader.iterator }
      val operation = Ceiling(Average(Pluck(10, csvIterator)))
      for (element <- operation)
        println(s"Results ${element}")
    } catch {
      case NonFatal(e) => println(s"error while parse file. $e")
    } finally {
      reader.close()
    }
  }
}

object Pluck {
  def apply(index: Int, stringReader: Iterable[Seq[String]]) = new Pluck(index, stringReader)
}
class Pluck(private val index: Int, stringReader: Iterable[Seq[String]]) extends Iterable[Seq[String]] {
  val stringIterator = stringReader.iterator

  def iterator: Iterator[Seq[String]] = new Iterator[Seq[String]] {
    def hasNext = stringIterator.hasNext
    def next = {
      val res = stringIterator.next()(index)
      println(s"get next Pluck($index) $res")
      Seq(res)
    }
  }
}

object Ceiling {
  def apply(stringReader: Iterable[Seq[String]]) = new Ceiling(stringReader)
}
class Ceiling(stringReader: Iterable[Seq[String]]) extends Iterable[Seq[String]] {
  var started = false
  val stringIterator = stringReader.iterator

  def iterator: Iterator[Seq[String]] = new Iterator[Seq[String]] {
    def hasNext = stringIterator.hasNext && ! started
    def next = {
      val firstElement = stringIterator.next()
      println(s"get next val for calculate ceiling $firstElement")
      val ceil = math.ceil(firstElement.head.toDouble).toInt
      println(s"ceil is $ceil")
      Seq(ceil.toString)
    }
  }
}

object Average {
  def apply(stringReader: Iterable[Seq[String]]) = new Average(stringReader)
}
class Average(stringReader: Iterable[Seq[String]]) extends Iterable[Seq[String]] {
  private def parseDouble(s: String) = try { Some(s.toDouble) } catch { case NonFatal(_) => None }

  val stringIterator = stringReader.iterator
  var started = false
  var rowsCounter: Double = 0
  var accumulator: Double = 0

  def iterator: Iterator[Seq[String]] = new Iterator[Seq[String]] {
    def hasNext = stringIterator.hasNext && ! started
    def next = {
      for(v <- stringIterator) {
        parseDouble(v(0)).foreach { value =>
          println(s"get next val for calculate average $value")
          rowsCounter += 1
          accumulator += value
        }
      }

      started = true
      val result = accumulator / rowsCounter
      println(s"average is $result")
      Seq(result.toString)
    }
  }
}

//object Filter {
//  def apply(index: Int, stringReader: Iterable[Seq[String]]) = new Filter(index, stringReader)
//}
//class Filter(private val index: Int, private val value: Double, stringReader: Iterable[Seq[String]]) extends Iterable[Seq[String]] {
//  val stringIterator = stringReader.iterator
//
//  def getNextElement: Option[String] = {
//    if(stringIterator.hasNext {
//
//    })
//  }
//
//  def iterator: Iterator[Seq[String]] = new Iterator[Seq[String]] {
//    def hasNext = stringIterator.hasNext
//    def next = {
//      val res = stringIterator.next()(index)
//      println(s"get next Pluck($index) $res")
//      Seq(res)
//    }
//  }
//}
