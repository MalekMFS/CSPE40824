package CSPE40824.hw1

import CSPE40824.hw1.EventType.{Arrival, Done, Overdue}

import scala.collection.mutable
import scala.math.log
import scala.math.BigDecimal.double2bigDecimal

object Modeler {
  //TODO convert to Long instead of Bigdecimal
  val expDist: (Double, Double) => Double =
    (x: Double, lam: Double) => -log(1 - x) / lam

  def simulation (totalCust: Int, k: Int, mu: Int, theta: Int, lambda: Double, debug: Boolean = false): (Int, Int, Int) = {
    var queue     = new mutable.Queue[Customer](k)  // server queue limited to k. first is being served
    var events    = mutable.PriorityQueue.empty(MinOrder)   // events list.
    val r         = scala.util.Random          // Random number generator. use: r.nextDouble
    var time:Double= 0.0
    var nBlocked:Int   = 0                     // #customers encountered full queue
    var nOverdue:Int   = 0                     // #customers left due to Deadline (theta)
    var nDone:Int   = 0                        // #customers Done and got service

    /** create a list of Customers and their arrival, service, and wait time.
     * generates arrival times, using expDist summing with previous arrival.
     *
     * Create Arrival event for all users.*/
    val randTimes: Iterator[Double] = List
      .fill(totalCust)(expDist(r.nextDouble(), lambda))
      .scan(0.0)(_+_)
      .iterator
    val customers: Map[Int, Customer]   = randTimes.zipWithIndex.map {
      case (arrive, index) =>
        (index,
          Customer(arrive,
            expDist(r.nextDouble(), mu),
            theta,
            index))
    }.toMap
    //  events ++= customers.values.map(c => Event(Arrival, c.arriveT, c.id))
    val tempSorted = mutable.PriorityQueue.empty(MinOrder)
    tempSorted ++= customers.values.map(c => Event(Arrival, c.arriveT, c.id))
    val aEvents = tempSorted.iterator
    events += aEvents.next()

    /** Main Loop */
    while(events.nonEmpty){
      if (debug)
        println(f"> Queue: ${queue.size} | Events: ${events.size}")
      val e = events.dequeue()
      time = e.time
      if (debug)
        println(f"${e.eType} | Customer: ${e.custId} | Time: ${e.time}")


      e.eType match {
        case Arrival =>
          val newCust = customers(e.custId)

          if (queue.size == k) nBlocked += 1
          else if(queue.size < k && queue.nonEmpty){
            queue  += newCust
            events += Event(Overdue, time + newCust.waitT, newCust.id)
          }
          else if(queue.isEmpty){
            queue  += newCust
            events += Event(Done, time + newCust.serviceT, newCust.id)
          }

        case Overdue =>
          queue = queue.filter(_.id != e.custId); nOverdue += 1

        case Done =>
          nDone += 1
          queue.dequeue()
          if (queue.nonEmpty) {
            val current = queue.head
            events = events.filter(a =>
              a.custId != current.id || a.eType != Overdue) // remove user overdue event
            events += Event(Done, time + current.serviceT, current.id)
          }
      }
      if (events.size < totalCust && aEvents.hasNext) events += aEvents.next() //FIXME is this correct? maybe check time with last event
    }
    (nBlocked, nOverdue, nDone)
  }

  def analysis(k: Int, mu: Int, theta: Int, lambda: BigDecimal): Unit = {
    def fact(n: Int): Int = if (n == 1) 1 else n * fact(n-1)
    def Pii(f: Int =>  BigDecimal, n: Int):BigDecimal =
      if(n < 0) 1 else f(n) * f(n - 1)
    val expRo = (n: Int) =>
      fact(n) / Pii((i: Int) => mu + i/BigDecimal(theta),n)

    val exRoVals = (1 to k).map(expRo)
    println()

  }

}

object MinOrder extends Ordering[Event] {
  override def compare(x: Event, y: Event): Int = y.time.compareTo(x.time)
}