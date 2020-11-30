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
  //TODO use args for switching between Simulation modes, and Analysis mode
  //TODO fill readme file
  /** for Charts: lambda from 0.1 to 20.0 with 0.1 steps  */
  //FIXME for app: lambda = 5, 10, 15, and fixed wait time

  val params    = file"src/main/resources/parameters.conf".lines.map(_.toDouble).take(2) //FIXME file location for TA test
  val theta     = params.head                // waiting time. TWO MODES: fixed and exp
  val mu        = params.tail.head           // server service rate
  val lambdas   = 0.1 to 20.0 by 0.1         // entrance rates (poisson param).
//  val lambdas   = List(10.0)
  val totalCust = pow(10, 7) .toInt          // FIXME 10^7 or 10^8
  val k         = 12                         // Queue size

  val fixedOut: File = file"fixed.txt"
  fixedOut < "" // clear the file

  lambdas.foreach { lambda =>
      val (nBlocked, nOverdue, nDone) = {
        Modeler.simulation(totalCust, k, mu, theta, lambda.toDouble, false, false)
      }

      println(f"Overdues: $nOverdue | Blocked: $nBlocked | Done: $nDone")
      val pb = nBlocked.toDouble / totalCust
      val pd = nOverdue.toDouble / totalCust
      println(f"pb: $pb | pd: $pd | lambda: $lambda | totalCustomers: $totalCust")

      fixedOut << f"$pb $pd"
  }

  val expOut: File = file"exp.txt"
  expOut < "" // clear the file

  lambdas.foreach { lambda =>
      val (nBlocked, nOverdue, nDone) = {
        Modeler.simulation(totalCust, k, mu, theta, lambda.toDouble, true, false)
      }

      println(f"Overdues: $nOverdue | Blocked: $nBlocked | Done: $nDone")
      val pb = nBlocked.toDouble / totalCust
      val pd = nOverdue.toDouble / totalCust
      println(f"pb: $pb | pd: $pd | lambda: $lambda | totalCustomers: $totalCust")

    expOut << f"$pb $pd"
  }


//  val expAnalysisOut: File = file"expAnalysis.txt"
//  val fixedAnalysisOut: File = file"fixedAnalysis.txt"
//  expAnalysisOut < ""; fixedAnalysisOut < "" // make or clear the file
//
//  lambdas.map { lambda =>
//    val pbPd: Map[String, List[Double]] = Modeler.analysis(k, mu, theta, lambda.toDouble)
//    val expRes = pbPd("exp")
//    val fixedRes = pbPd("fixed")
//    expAnalysisOut   << f"${expRes.head} ${expRes.tail.head}"
//    fixedAnalysisOut << f"${fixedRes.head} ${fixedRes.tail.head}"
//  }

}