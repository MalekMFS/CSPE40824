package CSPE40824

case class Config(

     expOfCust: Int = 7,
     queueSize: Int = 12,
     examinerRun: Boolean = false,
     debug: Boolean = false,
     mode: String = "fixed", //Analysis, Simulation with fixed or exp theta
     queueMode: String = "fifo" // FIFO, Processor Sharing (Ideal Round Robin), or Discriminatory Processor Sharing
   )
