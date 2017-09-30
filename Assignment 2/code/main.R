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
	for(i in 1:NWATERH)
	{
		# Check the whole edges matrix
		buff <- i
		for(j in 1:dim(edges)[1])
		{
			# Detected a vertice to another node
			if(edges[j,1]==i)
				buff <- rbind(buff, edges[j,2])		
			if(edges[j,2]==i)
				buff <- rbind(buff, edges[j,1])
		}	
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
			#print(gauss[[j]][i,1])
			#print(gauss[[j]][i,2])
			#print(EM[i,j])
			#print(readings[j])		
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
	print(SM)
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

#############################################################################################
#
# Function to interface with the runWheresCroc function
#
#############################################################################################
ourFunction <- function(mnm, readings, positions, edges, gauss)
{
	if( !exists("state", where=mnm$mem) )
	{
		# Initialize ...
		# State matrix
		mnm$mem$SM <- matrix(1/NWATERH, ncol=NWATERH, nrow=1)
		# Transition matrix
		mnm$mem$TM <- makeTransitionM(edges)
		# State
		mnm$mem$state <- "WAIT"
		print(mnm$mem$TM)
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
		highest <- mostLikelyWH(SM)
		print(highest)	
	}
	mnm$moves <- c(0,0)
	readline(prompt="Well we are waiting :D")
	return(mnm)
}

runWheresCroc(ourFunction, showCroc = T, pause = 0)
