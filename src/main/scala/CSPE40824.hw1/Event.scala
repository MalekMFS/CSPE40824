package CSPE40824.hw1

//TODO add a customer blocked event?
object EventType extends Enumeration {
  type EventType = Value
  /** types:customer arrival, customer left (due to deadline), customer left from server */
  val Arrival, Overdue, Done = Value
}

/** att: type:[customer arrival, customer exit (aka deadline), customer exit from server], occurrence time, customer id */
import CSPE40824.hw1.EventType.EventType
case class Event(eType: EventType, time: Double, custId: Int)
