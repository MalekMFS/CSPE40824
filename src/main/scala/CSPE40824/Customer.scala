package CSPE40824

/** Att: arrival time, wait time before leaving (deadline), needed service time, id */
case class Customer(arriveT: Double, serviceT: Double, waitT: Double, id: Int, uClass: Byte = 1)
