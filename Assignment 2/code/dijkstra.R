#############################################################################################
#
# Project:	Assignment 1, Artificial Intelligence, Uppsala University
# Author: 	Maximilian Stiefel
# Last mod.:	01.10.2017
# File:		dijkstra.R 
# Description:	Implementing Dijkstra's algorithm for the current problem
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

#############################################################################################
# Global variables
#############################################################################################
NWATERH <- 40


############################################################################################
#
# Function to backtrace the Dijkstra algortihm
#
#############################################################################################
backtraceDijkstra <- function(goal, predecessorVertex)
{
	sequence <- c()
	currentVertex <- goal
	while( !is.na(predecessorVertex[currentVertex]) )
	{
		sequence <- rbind(sequence, currentVertex)
		currentVertex <- predecessorVertex[currentVertex]	
	}
	return(rev(sequence))
}

############################################################################################
#
# Function to implement Dijkstra's algorithm for the given problem
#
#############################################################################################
dijkstrasAlgo <- function(edges, start, goal)
{
	# 0 vertex has not been discovered, 1 vertex has been discovered
	statusVertex <- matrix(0, nrow=1, ncol=NWATERH)
	
	# Costs from goal to a vertex
	costVertex <- matrix(Inf, nrow=1, ncol=NWATERH)
	# Initialize start node
	costVertex[start] <- 0

	# Predecessor of each vertex
	predecessorVertex <- matrix(NA, nrow=1, ncol=NWATERH)

	# While not all vertices have been explored keep going
	while( !identical(statusVertex, matrix(1, nrow=1, ncol=NWATERH) ) )
	{
		# Find vertex with the lowest costs
		lowest <- c(0, Inf)
		for(i in 1:NWATERH)
		{
			if( (costVertex[i] < lowest[2]) && (statusVertex[i] == 0) )
			{
				lowest[1] <- i
				lowest[2] <- costVertex[i]
			}
		}
		currentVertex <- lowest[1]
		# Target found!
		if(currentVertex == goal)
			break;
		# Mark this vertex as discovered
		statusVertex[currentVertex] <- 1
		for(i in findNeighbours(edges, currentVertex))
		{
			tentative_score <- costVertex[currentVertex] + 1
			if( tentative_score < costVertex[i] )
			{
				costVertex[i] <- tentative_score
				predecessorVertex[i] <- currentVertex
			}	
		}	
	}
	return( backtraceDijkstra(goal, predecessorVertex) )
}	
