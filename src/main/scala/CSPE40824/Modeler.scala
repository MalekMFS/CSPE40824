package CSPE40824

import CSPE40824.EventType.{Arrival, Done, Overdue}

import scala.collection.mutable
import scala.math.{E, log, pow}

object Modeler {

  val expDist: (Double, Double) => Double =
    (x: Double, lam: Double) => -log(1 - x) / lam

  def simulation (totalCust: Int, k: Int, mu: Double, theta: Double, lambda: Double, expTheta: Boolean, queueMode: String, debug: Boolean = false): (Int, Int, Int) = {
    var queue     = new mutable.Queue[Customer](k)  // server queue limited to k. first is being served in FIFO mode
    var events    = mutable.PriorityQueue.empty(MinOrder)   // events list.
    val r         = scala.util.Random          // Random number generator. use: r.nextDouble
    var time:Double= 0.0
    var nBlocked:Int   = 0                     // #customers encountered full queue
    var nOverdue:Int   = 0                     // #customers left due to Deadline (theta)
    var nDone:Int   = 0                        // #customers Done and got service


    def generateCustomer(arrivalTime: Double, id: Int, classless: Boolean = true): Customer ={
      Customer(
        arrivalTime,
        expDist(r.nextDouble(), mu),
        if(expTheta) expDist(r.nextDouble(), 1/theta) else theta,
        id,
        if(classless) 1 else r.between(1, 3).toByte // in [1, 3)
      )
    }

    queueMode match {
      case "fifo" =>
        var i = 1
        events += Event(Arrival, time , i)
        /** Main Loop */
        while(nBlocked+nDone+nOverdue < totalCust){
          if (debug)
            println(f"> Queue: ${queue.size} | Events: ${events.size}")
          val e = events.dequeue()
          time = e.time
          if (debug)
            println(f"${e.eType} | Customer: ${e.custId} | Time: ${e.time}")

          e.eType match {
            case Arrival =>
              val customer = generateCustomer(time, e.custId)

              if (queue.size == k) nBlocked += 1
              else if(queue.isEmpty){
                queue  += customer
                events += Event(Done, time + customer.serviceT, customer.id)
              }
              else { // (queue.size < k && queue.nonEmpty)
                queue  += customer
                events += Event(Overdue, time + customer.waitT, customer.id)
              }
              if (i < totalCust){
                i += 1
                events += Event(Arrival, time + expDist(r.nextDouble(), lambda), i) // always have the next Arrival
              }

            case Overdue =>
              queue.dequeueFirst(_.id == e.custId); nOverdue += 1

            case Done =>
              nDone += 1
              queue.dequeue()
              if (queue.nonEmpty) {
                val current = queue.head
                events = events.filterNot(a =>
                  a.custId == current.id && a.eType == Overdue) // remove user overdue event
                events += Event(Done, time + current.serviceT, current.id)
              }
          }
        }

        /** This mode has No Done event */
      case "ps" =>
        var i = 1
        events += Event(Arrival, time , i)

        /** Main Loop */
        while(nBlocked+nDone+nOverdue < totalCust){
          if (debug)
            println(f"> Queue: ${queue.size} | Events: ${events.size}")
          val e = events.dequeue()
          if (debug)
            println(f"${e.eType} | Customer: ${e.custId} | Time: ${e.time}")

          /** compute remaining service time for customers in the queue */
          val queueSize = queue.size
          if (queueSize > 0) {
            //FIXME to increase the accuracy, time must also iterate to the next DONE event (exact zero service time left).
            queue = queue.map(c => Customer(c.arriveT, c.serviceT - (mu/queueSize) * (e.time - time), c.waitT, c.id))
            var doneCustomers = mutable.ArrayBuffer[Customer]()
            queue.foreach { c =>
              if (c.serviceT <= 0.0) {
                doneCustomers += c
                nDone += 1
                events = events.filterNot(_.custId == c.id ) // remove user's overdue event
              }
            }
            // Remove finished Customers from the queue
            if (doneCustomers.nonEmpty) doneCustomers.foreach( c => queue.dequeueFirst(_.id == c.id) )
          }
          time = e.time // Update time

          e.eType match {

            case Arrival =>
              if (queue.size == k) nBlocked += 1
              else {
                // (queue.size < k)
                val customer = generateCustomer(time, e.custId)
                queue  += customer
                events += Event(Overdue, time + customer.waitT, customer.id)
              }
              if (i < totalCust){
                i += 1
                events += Event(Arrival, time + expDist(r.nextDouble(), lambda), i) // always have the next Arrival
              }

            case Overdue =>
              val customer = queue
                .dequeueFirst(_.id == e.custId)
              customer match {
                /** Customer didn't get remove from the queue. So this is an Overdue */
                case Some(_) => nOverdue += 1

                /** Current event is an Overdue but the Customer already finished, removed from the queue, and nDone inc */
                case None => None
              }
          }
        }

      case "dps" =>
        var i = 1
        events += Event(Arrival, time , i)

        /** Main Loop */
        while(nBlocked+nDone+nOverdue < totalCust){
          if (debug)
            println(f"> Queue: ${queue.size} | Events: ${events.size}")
          val e = events.dequeue()
          if (debug)
            println(f"${e.eType} | Customer: ${e.custId} | Time: ${e.time}")

          /** compute remaining service time for customers in the queue */
          val queueSize = queue.size
          if (queueSize > 0) {
            val nClass2 = queue.count(_.uClass == 2)
            val weightsSum = (queueSize - nClass2) + (nClass2 * 2)
            //FIXME to increase the accuracy, time must also iterate to the next DONE event (exact zero service time left).
            queue = queue.map(c => Customer(c.arriveT, c.serviceT - (mu * c.uClass /weightsSum) * (e.time - time), c.waitT, c.id))
            var doneCustomers = mutable.ArrayBuffer[Customer]()
            queue.foreach { c =>
              if (c.serviceT <= 0.0) {
                doneCustomers += c
                nDone += 1
                events = events.filterNot(_.custId == c.id ) // remove user's overdue event
              }
            }
            // Remove finished Customers from the queue
            if (doneCustomers.nonEmpty) doneCustomers.foreach( c => queue.dequeueFirst(_.id == c.id) )
          }
          time = e.time // Update time

          e.eType match {

            case Arrival =>
              if (queue.size == k) nBlocked += 1
              else {
                // (queue.size < k)
                val customer = generateCustomer(time, e.custId, false)
                queue  += customer
                events += Event(Overdue, time + customer.waitT, customer.id)
              }
              if (i < totalCust){
                i += 1
                events += Event(Arrival, time + expDist(r.nextDouble(), lambda), i) // always have the next Arrival
              }

            case Overdue =>
              val customer = queue
                .dequeueFirst(_.id == e.custId)
              customer match {
                /** Customer didn't get remove from the queue. So this is an Overdue */
                case Some(_) => nOverdue += 1

                /** Current event is an Overdue but the Customer already finished, removed from the queue, and nDone inc */
                case None => None
              }
          }
        }

    }
    (nBlocked, nOverdue, nDone)
  }

  def analysis(k: Int, mu: Double, theta: Double, lambda: Double, queueMode: String): Map[String, List[Double]] = {

    def fact(n: Int): Int = if (n <= 1) 1 else n * fact(n-1)
    var pbExp, pbFixed, pdExp, pdFixed   = 0.0

    queueMode match {

      case "fifo" =>
        val expRo = (n: Int) =>
          fact(n) / (0 to n).map(i => mu + i / theta).product
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

        pbExp   = pnExp(k)
        pbFixed = pnFixed(k)

        /** using last formula */
        pdExp   = 1 - (mu/lambda) * (1 - p0Exp)   - pbExp
        pdFixed = 1 - (mu/lambda) * (1 - p0Fixed) - pbFixed

      case "ps" =>
        val expGama = (n: Int) =>
          n compareTo 0 match {
            case 0 => 0
            case 1 => n / theta
          }
        val fixedGama = (n: Int) =>
          n compareTo 0 match {
            case 0 => 0
            case 1 => mu / (pow( E, (mu * theta) / n ) - 1)
          }
        def Pn (n: Int, gama: Int => Double, p0: Double): Double =
          if (n >= 1)
            (pow(lambda,n) / (1 to n).map(i => mu + gama(i)).product) * p0
          else 0 // No n below 1
        def fP0 (gama: Int => Double): Double =
          pow(1 + (1 to k).map( i => pow(lambda, i) / (1 to i).map(j => mu + gama(j)).product ).sum,
            -1)

        val p0Exp   = fP0(expGama)
        val p0Fixed = fP0(fixedGama)

        val pnExp   = p0Exp   +: (1 to k).map(n => Pn(n, expGama, p0Exp))
        val pnFixed = p0Fixed +: (1 to k).map(n => Pn(n, fixedGama, p0Fixed))

        pbExp   = pnExp(k)
        pbFixed = pnFixed(k)

        /** using last formula */
        pdExp   = 1 - (mu/lambda) * (1 - p0Exp)   - pbExp
        pdFixed = 1 - (mu/lambda) * (1 - p0Fixed) - pbFixed
    }

    Map("exp" -> List(pbExp, pdExp), "fixed" -> List(pbFixed, pdFixed))
  }

}

object MinOrder extends Ordering[Event] {
  override def compare(x: Event, y: Event): Int = y.time.compareTo(x.time)
}