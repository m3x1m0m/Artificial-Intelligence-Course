#############################################################################################
#
# Project:	Assignment 2, Artificial Intelligence, Uppsala University
# Author: 	Maximilian Stiefel
# Last mod.:	01.10.2017
# File:		main.R 
# Description:	Main
#
#############################################################################################

#############################################################################################
# Libraries
#############################################################################################
library(DeliveryMan)

#############################################################################################
# Source files
#############################################################################################
source('ourFunction.R')

#############################################################################################
# Function to test our code
#############################################################################################
averageTest <- function(tests){
	sum = 0
	for (i in 1:tests) 
	{
		sum <- sum + runWheresCroc(ourFunction, showCroc = T, pause = 0) 		 
	        if(i%%10==0)
		{
			print(i)
			print(sum/i)
		}
	}
	cat("Average number of turns\n")
	print(sum/i)
	cat("After\n")
	print(tests)
	cat("tests\n")
	return(0)
}

averageTest(500)
