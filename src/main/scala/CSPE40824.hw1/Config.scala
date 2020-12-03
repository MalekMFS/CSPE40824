package CSPE40824.hw1

case class Config(
     expOfCust: Int = 7,
     queueSize: Int = 12,
     examinerRun: Boolean = false,
     debug: Boolean = false,
     mode: String = "fixed", //Analysis, Simulation with fixed or exp theta
   )
