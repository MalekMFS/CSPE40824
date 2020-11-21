package CSPE40824.hw1

//TODO add a status attribute?
/** Att: arrival time, wait time before leaving (deadline), needed service time, id */
case class Customer(arriveT: Double, serviceT: Double, waitT: Double, id: Int)
