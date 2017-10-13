#############################################################################################
#
# Project:	Assignment 2, Artificial Intelligence, Uppsala University
# Author: 	Maximilian Stiefel
# Last mod.:	01.10.2017
# File:		hFunctions.R 
# Description:	Functions to solve some problems
#
#############################################################################################

#############################################################################################
# Libraries
#############################################################################################
library(DeliveryMan)

#############################################################################################
# Global variables
#############################################################################################
NWATERH <- 40 # Number of water holes
NOBSS <- 3 # Number of observer states

#############################################################################################
#
# Function to find neighbours 
#
#############################################################################################
findNeighbours <- function(edges, vertex)
{
	# Check the whole edges matrix
	buff <- vertex
	for(j in 1:dim(edges)[1])
	{
		# Detected an edge 
		if(edges[j,1]==vertex)
			buff <- rbind(buff, edges[j,2])
		if(edges[j,2]==vertex)
			buff <- rbind(buff, edges[j,1])
	}
	return(buff)
}

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
	for(i in 1:NWATERH)
	{
		# Find neighbors 
		buff <- findNeighbours(edges, i)
		nvertices <- length(buff)
		# Use gained knowledge to modify transition matrix
		for(m in buff)
		{
			TM[i, m] <- 1/nvertices
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
	EMT <- matrix(0, nrow=NWATERH, ncol=NOBSS)
	EM <- matrix(0, nrow=NWATERH, ncol=NWATERH)
	# For every waterhole convert salinity, phosphate and nitrogen to a probability
	for(i in 1:NWATERH)
	{
		for(j in 1:NOBSS)
		{
			EMT[i,j] <- dnorm(readings[j], gauss[[j]][i,1], gauss[[j]][i,2])
		}	
	}
	# The three variables are independent 
	# So P(A,B,C) = P(A) * P(B) * P(C) 
	EMT <- EMT[,1] * EMT[,2] * EMT[,3]
	for(i in 1:NWATERH)
	{
		EM[i,i] <- EMT[i]
	}
	return(EM)
}

#############################################################################################
#
# Function to normalize the state matrix
#
#############################################################################################
normalizeStateM <- function(SM)
{
	sum <- 0
	for(i in 1:NWATERH)
	{
		sum <- sum + SM[i]	
	}
	SM <- SM * (1/sum)
	return(SM)
}

#############################################################################################
#
# Function to create a distance matrix weighting acc. to distance
#
#############################################################################################
makeDistanceM <- function(edges, position)
{
	lowest <- Inf
	dist <- 0
	DMT <- matrix(1, nrow=NWATERH, ncol=1)
	DM <- matrix(0, nrow=NWATERH, ncol=NWATERH)
	for(i in 1:NWATERH)
	{
		dist <- length(dijkstrasAlgo(edges, position, i)) +1
		if(dist < lowest)
			lowest <- dist
		DMT[i] <- dist
	}
	DMT <- lowest/DMT
	for(i in 1:NWATERH)
	{
		DM[i,i] <- DMT[i]
	}
	return(DM)
}

#############################################################################################
#
# Function to determine which waterhole is most likely to be the location of croc
#
#############################################################################################
mostLikelyWH <- function(SM)
{
	highest <- c(0,0)
	for(i in 1:NWATERH)
	{
		# New highest probability?
		if(SM[i] > highest[1])
		{
			highest <- c(SM[i], i)
		}	
	}
	return(highest)
}
