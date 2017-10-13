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
	# German Shepherd
	mnm$mem$schaeferhund <- FALSE
	while(mnm$mem$schaeferhund != TRUE)
	{
		if( !exists("SM", where=mnm$mem) )
		{
			# Initialize ...
			# State matrix
			mnm$mem$SM <- matrix(1/NWATERH, ncol=NWATERH, nrow=1)
			# Transition matrix
			mnm$mem$TM <- makeTransitionM(edges)
			# Number of moves
			mnm$mem$nu <- 0
			# Moves
			mnm$moves <- c(0,0)
		}
		else
		{
			# DO NOT LOOSE ONE TURN
			mnm$mem$schaeferhund <- TRUE
			# Get transition matrix and status vector
			SM <- mnm$mem$SM
			TM <- mnm$mem$TM
			# Go through all positions and check whether the croc is eating sb.
			for(i in 1:length(positions))
			{
				# If TRUE sb has been eaten
				if( is.na(positions[i]) )
					next
				# If TRUE sb has been just eaten
				if(positions[i] < 0)
				{
					# Reset state vector
					SM <- matrix(0, ncol=NWATERH, nrow=1)
					SM[-positions[i]] <- 1
				}		
			}
			# Generate emission matrix from readings
			EM <- makeEmissionM(readings, gauss)
			# Perform forward algorithm
			# Take distance into account
			DM <- makeDistanceM(edges, positions[3])
			SM <- normalizeStateM(SM %*% TM %*% EM %*% DM)
			foundyou <- mostLikelyWH(SM)[2]
			sequence <- dijkstrasAlgo(edges, positions[3], foundyou)
			if(!is.null(sequence))
			{
				if(length(sequence) < 2)
				{
					SM[sequence[1]] <- 0
					mnm$moves <- c(sequence[1], 0)
				}
				else
				{
					mnm$moves <- c(sequence[1], sequence[2])
				}
			}
			else
			{
				SM[positions[3]] <- 0
				mnm$moves <- c(0, 0)
			}
			mnm$mem$SM <- normalizeStateM(SM)
		}
	}
	return(mnm)
}

