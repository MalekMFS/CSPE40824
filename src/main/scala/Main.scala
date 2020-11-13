import better.files._

/** Main Console */
object Main extends App{
  //val input = scala.io.StdIn.readInt()
  val params = file"src/main/resources/parameters.conf".lines.map(_.toInt).take(2)
  val mu    = params.head
  val theta = params.tail.head

  val r = scala.util.Random // r.nextFloat
}
