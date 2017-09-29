#############################################################################################
#
# Project:	Assignment 1, Artificial Intelligence, Uppsala University
# Author: 	Maximilian Stiefel
# Last mod.:	23.09.2017
# File:		ourFunction.R 
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

#############################################################################################
# Global variables
#############################################################################################
NWATERH <- 40 # Number of water holes
NOBSS <- 3 # Number of observer states

#############################################################################################
#
# Function to create a transition matrix for the given problem given the graph
#
#############################################################################################
makeTransitionM <- function(edges)
{
	# Initialize transition matrix TM
	TM <- matrix(0, nrow=NWATERH, ncol=NWATERH)

	# For every water hole
	for(i in 1:dim(edges)[1])
	{
		# Check the whole matrix
		buff <- c()
		for(j in 1:dim(edges)[1])
		{
			# Detected a vertice to another node
			if(edges[j,1]==i)
				buff <- rbind(buff, edges[j,2])		
			if(edges[j,2]==i)
				buff <- rbind(buff, edges[j,1])
		}	
		nvertices <- length(buff)
		# Use gained knowledge to create transition matrix
		for(m in buff)
		{
			TM[m, i] <- 1/nvertices
		}
	}
	return(TM)
}

#############################################################################################
#
# Function to create an emission matrix from the seen observations
#
#############################################################################################
makeEmissionM <- function(readings, gauss)
{
	EM <- matrix(0, nrow=40, ncol=3)
	for(i in 1:NWATERH)
	{
		for(j in 1:NOBSS)
		{
			EM[i,j] <- dnorm(readings[j], gauss[[j]][i,1], gauss[[j]][i,2])		
		}	
	}
	return(EM)
}

#############################################################################################
#
# Function to interface with the runWheresCroc function
#
#############################################################################################
ourFunction <- function(mnm, readings, positions, edges, gauss)
{
	print(makeEmissionM(readings, gauss))
	readline(prompt="Well we are waiting :D")
	#return(mnm)
}

runWheresCroc(ourFunction, showCroc = T, pause = 0.1)
