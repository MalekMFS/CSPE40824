# Makefile for running the program in Discriminatory Processor Sharing queue mode that required by examiner as third assingment
run:
	java -jar CSPE-40824-assembly-0.3.jar --expOfCust 8 --mode fixed --queueMode dps --examiner & \
	java -jar CSPE-40824-assembly-0.3.jar --expOfCust 8 --mode exp --queueMode dps --examiner & \
	wait; \
	echo "Check fixed.txt and exp.txt with 'pb pd pd1 pd2' columns"
