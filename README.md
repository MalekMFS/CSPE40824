# CSPE40824.hw1
M/M/1/K Simulation and Analysis Modeler for Computer System Performance Evaluation course assignment. 
Which returns Blocked jobs and Overdue (timeout) jobs Probability to be imported into a Spreadsheet.

## How to run
You could run the project with `sbt run "<parameters>"` or just run the .jar file followed by following parameters (without quotes).
Note that all parameters are optional and have default values. Here are examples with default values.
* `-e 7` or `--expOfCust 7`: Indicates number of Customers (Jobs) in terms of power of ten.
The default is 7 (i.e 10^7).
* `-k 12` or `--queueSize 12`: Server's queue size.
* `--examiner`: Runs simulation only for lambda (λ) = 5, 10, 15
* `--debug`: Prints out more info like Arrival, Done, and Overdue events.
* `-m fixed`,`--mode fixed`: Switches between `analysis` mode and Simulation mode with `fixed` or `exp` theta (wait time) distributions.

## Build Standalone .jar file
` sbt assembly` 

## Dependencies
* [sbt](https://github.com/sbt/sbt): Scala build tool.
* [sbt-assembly](https://github.com/sbt/sbt-assembly): To make standalone (fat) .jar file.
* [better-files](https://github.com/pathikrit/better-files): for I/O
* [scopt](https://github.com/scopt/scopt): for command line options parsing.