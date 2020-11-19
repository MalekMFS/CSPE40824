package CSPE40824.hw1

import CSPE40824.hw1.EventType.{Arrival, Done, Overdue}

import scala.collection.mutable
import scala.math.log

object Modeler {
  val expDist: (Double, BigDecimal) => BigDecimal = (x: Double, lam: BigDecimal) => -log(1 - x) / lam

  def simulation (totalCust: Int, k: Int, mu: Int, theta: Int, lambda: BigDecimal): (Int, Int, Int) = {
    var queue     = mutable.Queue[Customer]()  // server queue. first is being served
    var events    = mutable.PriorityQueue.empty(MinOrder)   // events list.
    val r         = scala.util.Random          // Random number generator. use: r.nextDouble
    var time:BigDecimal= 0.0
    var nBlocked:Int   = 0                     // #customers encountered full queue
    var nOverdue:Int   = 0                     // #customers left due to Deadline (theta)
    var nDone:Int   = 0                        // #customers Done and got service

    /** create a list of Customers and their arrival, service, and wait time.
     * generates arrival times, using expDist summing with previous arrival.
     *
     * Create Aririval event for all users.*/
    val randTimes: Iterator[BigDecimal] = List.fill(totalCust)( expDist(r.nextDouble(), lambda) ).scan(BigDecimal("0.0"))(_+_).iterator
    val customers: Map[Int, Customer]   = randTimes.zipWithIndex.map { case (arrive, index) =>
      ( index, Customer(arrive, expDist(r.nextDouble(), mu), theta, index) ) }.toMap
    //  events ++= customers.values.map(c => Event(Arrival, c.arriveT, c.id))
    val tempSorted = mutable.PriorityQueue.empty(MinOrder); tempSorted ++= customers.values.map(c => Event(Arrival, c.arriveT, c.id))
    val aEvents = tempSorted.iterator
    events += aEvents.next()

    /** Main Loop */
    while(events.nonEmpty){
      println(f"> Queue: ${queue.size} | Events: ${events.size}")
      val e = events.dequeue()
      time = e.time
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
          if (queue.nonEmpty){
            val current = queue.head
            events = events.filter(a => a.custId != current.id || a.eType != Overdue) // remove user overdue event
            events += Event(Done, time + current.serviceT, current.id)
          }
      }
      if (events.size < totalCust && aEvents.hasNext) events += aEvents.next() //FIXME is this correct? maybe check time with last event
    }
    (nBlocked, nOverdue, nDone)
  }
}

object MinOrder extends Ordering[Event] {
  override def compare(x: Event, y: Event): Int = y.time.compareTo(x.time)
}