package CSPE40824.hw1


import better.files._
import better.files.Dsl.SymbolicOperations
import scopt.OParser

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
        .text("Run only for lambda = 5, 10, 15 and fixed wait time for examiner"),
      opt[Unit]("debug")
        .action((_, c) => c.copy(debug = true))
        .text("Shows more output for debug purposes"),
      opt[String]('m',"mode")
        .action((x,c) => c.copy(mode = x))
    )
  }

  /** for Charts: lambda from 0.1 to 20.0 with 0.1 steps
   * for app: lambda = 5, 10, 15, and fixed wait time */
  // OParser.parse returns Option[Config]
  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      val params    = file"src/main/resources/parameters.conf".lines.map(_.toDouble).take(2) //FIXME file location for TA test
      val theta     = params.head                // waiting time. TWO MODES: fixed and exp
      val mu        = params.tail.head           // server service rate
      val lambdas   = if (config.examinerRun) 5.0 to 15.0 by 5.0
        else 0.1 to 20.0 by 0.1                  // entrance rates (poisson param).
      //  val lambdas   = List(10.0)
      val totalCust = pow(10, config.expOfCust) .toInt
      val k         = config.queueSize           // Queue size

      config.mode match {
        case "fixed" =>
          println(".:Simulation mode with Fixed Waiting times:.\n Output will be overridden to fixed.txt")
          val fixedOut: File = file"fixed.txt"
          fixedOut < "" // clear the file

          lambdas.foreach { lambda =>
            val (nBlocked, nOverdue, nDone) = {
              Modeler.simulation(totalCust, k, mu, theta, lambda.toDouble, false, config.debug)
            }

            println(f"Overdues: $nOverdue | Blocked: $nBlocked | Done: $nDone")
            val pb = nBlocked.toDouble / totalCust
            val pd = nOverdue.toDouble / totalCust
            println(f"pb: $pb | pd: $pd | lambda: $lambda | totalCustomers: $totalCust")

            fixedOut << f"$pb $pd"
          }

        case "exp"   =>
          println(".:Simulation mode with Exponential Waiting times:.\n Output will be overridden to exp.txt")
          val expOut: File = file"exp.txt"
          expOut < "" // clear the file

          lambdas.foreach { lambda =>
            val (nBlocked, nOverdue, nDone) = {
              Modeler.simulation(totalCust, k, mu, theta, lambda.toDouble, true, config.debug)
            }

            println(f"Overdues: $nOverdue | Blocked: $nBlocked | Done: $nDone")
            val pb = nBlocked.toDouble / totalCust
            val pd = nOverdue.toDouble / totalCust
            println(f"pb: $pb | pd: $pd | lambda: $lambda | totalCustomers: $totalCust")

            expOut << f"$pb $pd"
          }

        case "Analysis" =>
          println(".:Analysis mode:.\n Output will be overridden to expAnalysis.txt and fixedAnalysis.txt")
          val expAnalysisOut: File = file"expAnalysis.txt"
          val fixedAnalysisOut: File = file"fixedAnalysis.txt"
          expAnalysisOut < ""; fixedAnalysisOut < "" // make or clear the file

          lambdas.map { lambda =>
            val pbPd: Map[String, List[Double]] = Modeler.analysis(k, mu, theta, lambda.toDouble)
            val expRes = pbPd("exp")
            val fixedRes = pbPd("fixed")
            expAnalysisOut   << f"${expRes.head} ${expRes.tail.head}"
            fixedAnalysisOut << f"${fixedRes.head} ${fixedRes.tail.head}"
          }
      }

    case _ => println("bad arguments! try -h for list of arguments.")
  }
  //TODO fill readme file
  //TODO make MAKEFILE to run exp and fixed mode in parallel
}