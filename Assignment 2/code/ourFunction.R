#############################################################################################
#
# Project:	Assignment 2, Artificial Intelligence, Uppsala University
# Author: 	Maximilian Stiefel
# Last mod.:	01.10.2017
# File:		ourFunction.R 
# Description:	Implementation of the main routine to interface with the game
#
#############################################################################################

#############################################################################################
# Libraries
#############################################################################################
library(DeliveryMan)

#############################################################################################
# Source files
#############################################################################################
source('hFunctions.R')
source('dijkstra.R')

#############################################################################################
# Global variables
#############################################################################################
NWATERH <- 40 # Number of water holes
NOBSS <- 3 # Number of observer states

#############################################################################################
#
# Function to interface with the runWheresCroc function
#
#############################################################################################
ourFunction <- function(mnm, readings, positions, edges, gauss)
{
	if( !exists("SM", where=mnm$mem) )
	{
		# Initialize ...
		# State matrix
		mnm$mem$SM <- matrix(1/NWATERH, ncol=NWATERH, nrow=1)
		# Transition matrix
		mnm$mem$TM <- makeTransitionM(edges)
		# Moves
		mnm$moves <- c(0,0)
	}
	else
	{
		# Go through all positions and check whether the croc is eating sb.
		for(i in 1:length(positions))
		{
			# If TRUE sb has been eaten
			if( is.na(positions[i]) )
				next
			if(positions[i] < 0)
			{
				SM <- matrix(0, ncol=NWATERH, nrow=1)
				SM[-positions[i]] <- 1
			}		
		}
		# Get transition matrix and status vector
		SM <- mnm$mem$SM
		TM <- mnm$mem$TM
		# Generate emission matrix from readings
		EM <- makeEmissionM(readings, gauss)
		# Perform forward algorithm
		SM <- SM %*% TM %*% EM 
	       	SM <- normalizeStateM(SM)
		mnm$me$SM <- SM
		foundyou <- mostLikelyWH(SM)[2]
		sequence <- dijkstrasAlgo(edges, positions[3], foundyou)
		if(length(sequence) < 2)
		{
			mnm$moves <- c(sequence[1], 0)
		}
		else if(length(sequence) < 1) 
		{
			mnm$moves <- c(0, 0)
		}
		else
		{
			mnm$moves <- c(sequence[1], sequence[2])
		}
	}
	return(mnm)
}

