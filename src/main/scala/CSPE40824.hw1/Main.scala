package CSPE40824.hw1


import better.files._

import scala.collection.mutable
import scala.math.BigDecimal.double2bigDecimal
import scala.math.{log, pow} // Natural Log

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
  val r         = scala.util.Random          // Random number generator. use: r.nextFloat
  val totalCust = pow(10, 3) .toInt          //FIXME 10^7 or 10^8
  val expDist   = (x: Double, lam: BigDecimal) => -log(1 - x) / lam
  val k         = 12                         // Queue size

  var queue     = mutable.Queue[Customer]()  // server queue. first is being served
  var events    = mutable.ListBuffer[Event]()// events list.
//  var time:BigDecimal= 0.0                   // FIXME is time Int?
  var nBlocked  = 0                          // #customers encountered full queue
  var nOverdue  = 0                          // #customers left due to Deadline (theta)


  /** create a list of Customers and their arrival, service, and wait time. */
  // TODO other lambdas?
  // generate arrival times, using expDist summing with previous arrival.
  val randTimes: Iterator[BigDecimal] = List.fill(totalCust)( expDist(r.nextDouble(), lambda.head) ).scan(BigDecimal("0.0"))(_+_).iterator
  val customers: Iterator[Customer]   = randTimes.zipWithIndex.map { case (arrive, index) =>
                                           Customer(arrive, expDist(r.nextDouble(), mu), theta, index) }

  /** Main Loop */
  //TODO make sure the one have service won't removed because of past deadline.
  randTimes.foreach{ time =>
    // make events' list.   pre-define  or  calc through the loop?
    //Event:
    // 1. Customer Arrival
      // check queue size:
        // 1.1: Queue is full:
          // user blocked, inc nBlocked.
        // 1.2: Queue is not full and not empty (server is busy):
          // add customer to the queue, and add a arrival Event
          // calculate 'deadline = waitT + time' and add to the events? (to be removed if served)
        // 1.3: Queue is empty:
          // add customer to the queue, and add a arrival Event
          // calculate 'Done time = waitT + time' to make a Done Event and hold it in a proper place
    // 2. Customer overdue (deadline)
      // remove the customer from the queue and reorder the queue, and inc the nOverdue
    // 3. Customer Done due to getting the service
      // remove the customer from the queue, and add a Done event
      // if the queue is not empty:
        // ignore (remove) the deadline (event) of new served customer

    // process events
    // handle queue 'k' limitation and update blocked customer
  }

  //TODO calc pb and pd
  //TODO change the lambda in the loop
  //TODO remove the output file at the beginning. and append result of each lambda.
  //TODO add a iteration printer function for debugging purposes
  //TODO make new files for FIXED and EXP modes
}
