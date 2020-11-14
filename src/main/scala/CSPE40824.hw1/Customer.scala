package CSPE40824.hw1

//TODO add a status attribute?
/** Att: arrival time, wait time before leaving (deadline), needed service time, id */
case class Customer(arriveT: BigDecimal, serviceT: BigDecimal, waitT: Int, id: Int)
