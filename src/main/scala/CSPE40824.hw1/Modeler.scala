package CSPE40824.hw1

import CSPE40824.hw1.EventType.{Arrival, Done, Overdue}

import scala.collection.mutable
import scala.math.{E, log, pow}
import scala.math.BigDecimal.double2bigDecimal

object Modeler {
  //TODO convert to Long instead of Bigdecimal
  val expDist: (Double, Double) => Double =
    (x: Double, lam: Double) => -log(1 - x) / lam

  def simulation (totalCust: Int, k: Int, mu: Double, theta: Double, lambda: Double, debug: Boolean = false): (Int, Int, Int) = {
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

  def analysis(k: Int, mu: Int, theta: Int, lambda: Double): Map[String, List[Double]] = {
    def fact(n: Int): Int = if (n <= 1) 1 else n * fact(n-1)

    val expRo = (n: Int) =>
      fact(n) / (0 to n).map(i => mu + i / theta.toDouble).product
    val fixedRo = (n: Int) =>
      (fact(n).toDouble / pow(mu, n+1)) * (1 - pow(E, -(mu * theta)) * (0 to n-1).map(i => pow(mu*theta,i) / fact(i) ).sum)
    def Pn (n: Int, ro: Double): Double =
    // Each returned value must multiply with 'p0' to get 'pn'
      n compare 1 match {
        case 0 => lambda / mu
        case 1 => pow(lambda, n) * ro / fact(n - 1)
        case -1 => 1 // same as others must multiply to p0
      }

    val expRoVals = (0 to k).map(expRo) //NOTE the 0 is dummy and will not be used
    val fixedRoVals = (0 to k).map(fixedRo)

    val XExp =   (1 to k).map(n => Pn(n, expRoVals(n-1))) // changed -1 here because ro0 will not be calculated
    val XFixed = (1 to k).map(n => Pn(n, fixedRoVals(n-1)))
    val p0Exp   = 1 / (XExp.sum + 1.0)   // +1 to add p0
    val p0Fixed = 1 / (XFixed.sum + 1.0) // +1 to add p0

    val pnExp   = p0Exp   +: XExp.map(_ * p0Exp)
    val pnFixed = p0Fixed +: XFixed.map(_ * p0Fixed)

    val pbExp   = pnExp(k)
    val pbFixed = pnFixed(k)

    /** using last formula */
    val pdExp   = 1 - (mu/lambda) * (1 - p0Exp)   - pbExp
    val pdFixed = 1 - (mu/lambda) * (1 - p0Fixed) - pbFixed

    Map("exp" -> List(pbExp, pdExp), "fixed" -> List(pbFixed, pdFixed))
  }

}

object MinOrder extends Ordering[Event] {
  override def compare(x: Event, y: Event): Int = y.time.compareTo(x.time)
}