#############################################################################################
#
# Project:	Assignment 1, Artificial Intelligence, Uppsala University
# Author: 	Maximilian Stiefel
# Last mod.:	01.10.2017
# File:		hFunctions.R 
# Description:	Functions to help using some problems
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
	EM <- matrix(0, nrow=NWATERH, ncol=NOBSS)
	# For every waterhole convert salinity, phosphate and nitrogen to a probability
	for(i in 1:NWATERH)
	{
		for(j in 1:NOBSS)
		{
			EM[i,j] <- dnorm(readings[j], gauss[[j]][i,1], gauss[[j]][i,2])
		}	
	}
	# The three variables are independent 
	# So P(A,B,C) = P(A), P(B), P(C) 
	EM <- EM[,1] * EM[,2] * EM[,3]
	EMT <- matrix(0, nrow=NWATERH, ncol=NWATERH)
	for(i in 1:NWATERH)
	{
		EMT[i,i] <- EM[i]
	}
	return(EMT)
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
	SM <- SM/sum
	return(SM)
}

#############################################################################################
#
# Function to predict state that lies x steps in the future (Markov Chain)
#
#############################################################################################
predictState <- function(SM, TM, x)
{
		return(SM %*% TM^x)
}

#############################################################################################
#
# Function to determine which waterhole the croc is sitting after x turns
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

