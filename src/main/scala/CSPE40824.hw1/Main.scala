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
  var time:BigDecimal= 0.0                   //FIXME is time Int?
  var nBlocked  = 0                          // n left customers due to full queue
  var nLeft     = 0                          // n left customers due to Deadline (theta)


  /** create a list of Customers */
  //TODO other lambdas?
  val randTimes: Iterator[BigDecimal] = List.fill(totalCust)( expDist(r.nextDouble(), lambda.head) ).scan(BigDecimal("0.0"))(_+_).iterator
  val customers: Iterator[Customer]   = randTimes.map( arrive => Customer(arrive, expDist(r.nextDouble(), mu), theta) )

  /** Main Loop */
  // make events list. pre-define or through the loop?
  // process events
  // handle queue 'k' limitation and update blocked customer
  //TODO add a iteration printer function for debugging purposes
  //TODO make new files for FIXED and EXP modes
}
