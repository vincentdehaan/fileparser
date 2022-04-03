package nl.vindh.fileparser

import scala.collection.immutable.{Set => ISet}

object Temp {
  def maxEvents(arrival: Array[Int], duration: Array[Int]): Int = {
    val data = arrival.zip(duration)
    val filtered = filterSpanningEvents(data)
    power(filtered.toSet).filter(noOverlap).map(_.size).max
  }

  def power[A](t: ISet[A]): ISet[ISet[A]] = {
         @annotation.tailrec
         def pwr(t: ISet[A], ps: ISet[ISet[A]]): ISet[ISet[A]] =
             if (t.isEmpty) ps
           else pwr(t.tail, ps ++ (ps map (_ + t.head)))

       pwr(t, ISet(ISet.empty[A]))
       }

  def noOverlap(events: ISet[(Int, Int)]): Boolean =
    !(for {
      event <- events
      otherEvent <- events - event
    } yield noOverlap(event, otherEvent)).contains(false)

  def noOverlap(eventA: (Int, Int), eventB: (Int, Int)): Boolean =
    if(eventA._1 < eventB._1) eventA._1 + eventA._2 <= eventB._1
    else eventB._1 + eventB._2 <= eventA._1

  def filterSpanningEvents(events: Array[(Int, Int)]): Array[(Int, Int)] = { // remove event A if there is an event B that starts later and ends earlier
    val eventsWithIndex = events.zipWithIndex
    eventsWithIndex.filter {
      case ((startA, durA), idxA) => !eventsWithIndex.exists {
        case ((startB, durB), idxB) => idxA != idxB && startA <= startB && (startA + durA > startB + durB)
      }
    }.map(_._1)
  }

  println(filterSpanningEvents(Array((1, 2), (3, 2))).toList)
}

object Temp2 {
  def finalInstances(instances: Int, averageUtil: Array[Int]): Int = {
    averageUtil.foldLeft((instances, 10)) {
      case ((inst, lastChanged), nw) => {
        println(">>" + nw)
        if(lastChanged >= 9) {
          if ((nw > 60) && (inst <= (10 ^ 8))) {
            println("+" + inst); ((inst * 2), 0)
          }
          else if (nw < 25 && inst != 1) {
            println("-" + inst); ((inst.toFloat / 2).ceil.toInt, 0)
          }
          else {
            println("="); (inst, lastChanged + 1)
          }
        } else {
          println(">" + lastChanged)
          (inst, lastChanged + 1)
        }}
    }
  }._1

  println(finalInstances(5, Array(37,19,47,63,39,77,3,45,71,69,6,31,29,27,93,23,5,60,28,97,76)))
}