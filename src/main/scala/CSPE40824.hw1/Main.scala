package CSPE40824.hw1


import better.files._
import better.files.Dsl.SymbolicOperations

import scala.math.BigDecimal.double2bigDecimal
import scala.math.pow

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
  //TODO fill readme file
  /** for Charts: lambda from 0.1 to 20.0 with 0.1 steps  */
  /** for app: lambda = 5, 10, 15, and fixed wait time*/
  //TODO Check BigDecimal and Double problem
  val params    = file"src/main/resources/parameters.conf".lines.map(_.toInt).take(2) //FIXME file location for TA test
  val theta     = params.head                // waiting time. TWO MODES: fixed and exp
  val mu        = params.tail.head           // server service rate
  val lambdas   = BigDecimal("0.1") to BigDecimal("20.0") by BigDecimal("0.1") // entrance rates (poisson param).
  val totalCust = pow(10, 4) .toInt          // FIXME 10^7 or 10^8
  val k         = 12                         // Queue size

  val fixedOut: File = file"fixed.txt"
  fixedOut < "" // clear the file
  lambdas.foreach{ lambda =>
    val (nBlocked, nOverdue, nDone) = Modeler.simulation(totalCust, k, mu, theta, lambda)

    println(f"Overdues: $nOverdue | Blocked: $nBlocked | Done: $nDone")
    val pb = nBlocked / BigDecimal(totalCust)
    val pd = nOverdue / BigDecimal(totalCust)
    println(f"pb: $pb | pd: $pd | lambda: $lambda | totalCustomers: $totalCust")

    fixedOut << f"$pb $pd"
  }

  //TODO make 2 new files for FIXED and EXP modes
}