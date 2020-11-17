package CSPE40824.hw1


import better.files._
import CSPE40824.hw1.EventType.{Arrival, Done, Overdue}
import better.files.Dsl.SymbolicOperations

import scala.collection.mutable
import scala.math.BigDecimal.double2bigDecimal
import scala.math.{log, pow}

/** Main Console for FCFS queue.
 *
 * Return 'pb' and 'pd' in to text file with columns to be used in Excel.
 *
 * @note Customers have 3 final status: got service, blocked(queue was full), left queue due the deadline.
 *
 * - First entrance of customers could be 0. it is poisson with lambda (independent to next and before) the next.
 *
 * - Run in two theta modes: fixed and exp
 *
 * - If you need precise decimal calculations, the actually reliable way is to use BigDecimal: BigDecimal("0.05") to BigDecimal("0.95") by BigDecimal("0.05") It's a lot slower, so not acceptable in some contexts, but that's the reality of working with decimals on modern computers.*/
object Main extends App{
  //TODO add a .md readme
  /** for Charts: lambda from 0.1 to 20.0 with 0.1 steps  */
  /** for app: lambda = 5, 10, 15, and fixed wait time*/
  //TODO Check BigDecimal and Double problem
  val params    = file"src/main/resources/parameters.conf".lines.map(_.toInt).take(2) //FIXME file location for TA test
  val theta     = params.head                // waiting time. TWO MODES: fixed and exp
  val mu        = params.tail.head           // server service rate
  val lambda    = BigDecimal("0.1") to BigDecimal("20.0") by BigDecimal("0.1") // entrance rate (poisson param).
  val r         = scala.util.Random          // Random number generator. use: r.nextDouble
  val totalCust = pow(10, 4) .toInt          //FIXME 10^7 or 10^8
  val expDist   = (x: Double, lam: BigDecimal) => -log(1 - x) / lam
  val k         = 12                         // Queue size

  var queue     = mutable.Queue[Customer]()  // server queue. first is being served
  var events    = mutable.PriorityQueue.empty(MinOrder)   // events list.
  var time:BigDecimal= 0.0
  var nBlocked:Int   = 0                     // #customers encountered full queue
  var nOverdue:Int   = 0                     // #customers left due to Deadline (theta)


  /** create a list of Customers and their arrival, service, and wait time.
   * generates arrival times, using expDist summing with previous arrival.
   *
   * Create Aririval event for all users.*/
  // TODO loop over lambdas
  val randTimes: Iterator[BigDecimal] = List.fill(totalCust)( expDist(r.nextDouble(), lambda.head) ).scan(BigDecimal("0.0"))(_+_).iterator
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
        queue.dequeue()
        if (queue.nonEmpty){
          val current = queue.head
          events = events.filter(a => a.custId != current.id || a.eType != Overdue) // remove user overdue event
          events += Event(Done, time + current.serviceT, current.id)
        }
    }
    if (events.size < totalCust && aEvents.hasNext) events += aEvents.next() //FIXME is this correct? maybe check time with last event
  }
  println(f"Overdues: $nOverdue | Blocked: $nBlocked")
  val pd = nOverdue.toDouble / totalCust
  val pb = nBlocked.toDouble / totalCust
  println(f"pb: $pb | pd: $pd")
  //FIXME remove the file in each run
  val f1: File = file"fixed.txt"
  f1 << f"$pb $pd"

  //TODO change the lambda in the loop
  //TODO remove the output file at the beginning. and append result of each lambda.
  //TODO make 2 new files for FIXED and EXP modes
}

object MinOrder extends Ordering[Event] {
  override def compare(x: Event, y: Event): Int = y.time.compareTo(x.time)
}