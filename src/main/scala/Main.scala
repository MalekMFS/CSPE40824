import better.files._

import scala.collection.mutable
import scala.math.BigDecimal.double2bigDecimal

/** Main Console for FCFS queue.
 *
 * Return 'pb' and 'pd' in to text file with columns to be used in Excel.
 *
 * @note Customers have 3 final status: got service, blocked(queue was full), left queue due the deadline.
 *
 * First entrance of customers could be 0. it is poisson with lambda (independent to next and before) the next.
 *
 * Run in two theta modes: fixed and exp */
object Main extends App{
  /** for Charts: lambda from 0.1 to 20.0 with 0.1 steps  */
  /** for app: lambda = 5, 10, 15, and fixed wait time*/
  //TODO make new files for FIXED and EXP modes
  //val input = scala.io.StdIn.readInt()
  val params = file"src/main/resources/parameters.conf".lines.map(_.toInt).take(2) //FIXME location of file for TA test
  val theta  = params.head         // waiting time. TWO modes: fixed and exp
  val mu     = params.tail.head    // server service rate
  val lambda = 0.1 to 20.0 by 0.1  // entrance rate (poisson param).
  val x      = scala.util.Random   // Random number generator. use: x.nextFloat
  val k      = 12                  // Queue size
  var queue  = mutable.Queue()     //TODO handle as limited queue
  val totalCust = scala.math.pow(10, 3) //FIXME 10^7 or 10^8
  var time   = 0                   //FIXME is time Int?



  /** Main Loop */
  //TODO create a list of customers
}
