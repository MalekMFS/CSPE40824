package CSPE40824


import better.files._
import better.files.Dsl.SymbolicOperations
import scopt.OParser

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.math.BigDecimal.double2bigDecimal
import scala.math.pow

/** FCFS queue (M/M/1/K) Simulation and Analysis Modeling.
 *
 * Returns 'pb' and 'pd' in to text file in two columns to be imported in Excel.
 *
 * @note Customers have 3 final status: got service, blocked(queue was full), left queue due the deadline (Overdue).
 *
 * - First entrance of customers could be 0. it is poisson with lambda (independent to next and before) the next.
 *
 * - Simulation runs in two theta modes: fixed and exp
 *
 * - If you need precise decimal calculations, the actually reliable way is to use BigDecimal: BigDecimal("0.05") to BigDecimal("0.95") by BigDecimal("0.05") It's a lot slower, so not acceptable in some contexts, but that's the reality of working with decimals on modern computers.*/
object Main extends App{
  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("CSPE-hw1"),
      head("CSPE-hw1", "0.1"),
      opt[Int]('e', "expOfCust")
        .action((x,c) => c.copy(expOfCust = x))
        .text("Indicating number of customers to be 10^e"),
      opt[Int]('k', "queueSize")
        .action((x,c) => c.copy(queueSize = x))
        .text("Server's Queue Size"),
      opt[Unit]("examiner")
        .action((_, c) => c.copy(examinerRun = true))
        .text("Run only for lambda = 5, 10, 15 for examiner use"),
      opt[Unit]("debug")
        .action((_, c) => c.copy(debug = true))
        .text("Shows more output for debug purposes"),
      opt[String]('m',"mode")
        .action((x,c) => c.copy(mode = x))
        .text("Switch between 'analysis', Simulation with 'fixed' or 'exp' theta"),
      opt[String]('q', "queueMode")
        .action((x,c) => c.copy(queueMode = x))
        .text("Switch between 'fifo' or 'ps' queue serving mode")
    )
  }

  /** for Charts: lambda from 0.1 to 20.0 with 0.1 steps
   * for Examiner: lambda = 5, 10, 15 */
  // OParser.parse returns Option[Config]
  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      val params    = file"parameters.conf".lines.map(_.toDouble).take(2)
      val theta     = params.head                // waiting time. TWO MODES: fixed and exp
      val mu        = params.tail.head           // server service rate
      val lambdas   = if (config.examinerRun) 5.0 to 15.0 by 5.0
                      else 0.1 to 20.0 by 0.1    // entrance rates (poisson param).
      //  val lambdas   = List(10.0)
      val totalCust = pow(10, config.expOfCust) .toInt
      val k         = config.queueSize           // Queue size

      config.mode match {
        case "fixed" =>
          println(f".:Simulation mode with Fixed Waiting times and ${config.queueMode} queue:.\n Output will be overridden to fixed.txt")

          val fs = lambdas.map { lambda =>
            Future {
              Modeler.simulation(totalCust, k, mu, theta, lambda.toDouble, expTheta = false, config.queueMode, debug = config.debug)
            }
          }
          val f = Future.sequence(fs)
          val res = Await.result(f,Duration.Inf)
          var i = 0
          val fixedOut: File = file"fixed.txt"
          fixedOut.clear()
          res.foreach { case (nBlocked, nOverdue, nDone) =>
            val pb = nBlocked.toDouble / totalCust
            val pd = nOverdue.toDouble / totalCust
            println(f"pb: $pb | pd: $pd | lambda: ${lambdas(i)} | totalCustomers: $totalCust")

            fixedOut << f"$pb $pd"
            i += 1
          }

        case "exp"   =>
          println(s".:Simulation mode with Exponential Waiting times and ${config.queueMode} queue:.\n Output will be overridden to exp.txt")

          val fs = lambdas.map { lambda =>
            Future{
              Modeler.simulation(totalCust, k, mu, theta, lambda.toDouble, expTheta = true, config.queueMode, config.debug)
            }
          }
          val f = Future.sequence(fs)
          val res = Await.result(f,Duration.Inf)
          var i = 0
          val expOut: File = file"exp.txt"
          expOut.clear()
          res.foreach { case (nBlocked, nOverdue, nDone) =>
            val pb = nBlocked.toDouble / totalCust
            val pd = nOverdue.toDouble / totalCust
            println(f"pb: $pb | pd: $pd | lambda: ${lambdas(i)} | totalCustomers: $totalCust")

            expOut << f"$pb $pd"
            i += 1
          }

        case "analysis" =>
          println(f".:Analysis mode with ${config.queueMode} queue:.\n Output will be overridden to expAnalysis.txt and fixedAnalysis.txt")
          val expAnalysisOut: File = file"expAnalysis.txt"
          val fixedAnalysisOut: File = file"fixedAnalysis.txt"
          expAnalysisOut < ""; fixedAnalysisOut < "" // make or clear the file

          lambdas.map { lambda =>
            val pbPd: Map[String, List[Double]] = Modeler.analysis(k, mu, theta, lambda.toDouble, config.queueMode)
            val expRes = pbPd("exp")
            val fixedRes = pbPd("fixed")
            expAnalysisOut   << f"${expRes.head} ${expRes.tail.head}"
            fixedAnalysisOut << f"${fixedRes.head} ${fixedRes.tail.head}"
          }
      }

    case _ => println("bad arguments! try -h for list of arguments.")
  }
}