#############################################################################################
#
# Project:	Assignment 1, Artificial Intelligence, Uppsala University
# Author: 	Maximilian Stiefel
# Last mod.:	23.09.2017
# File:		ourFunction.R 
# Description:	Function to interface with the A* algorithm
#
#############################################################################################

#############################################################################################
# Libraries
#############################################################################################
library(DeliveryMan)

#############################################################################################
# Source files
#############################################################################################
source('aStar.R')
source('hFunctions.R')

#############################################################################################
# Global variables
#############################################################################################
dim = 10

#############################################################################################
#
# Function to interface with the delivery man game
#
#############################################################################################
ourFunction <- function(traffic, car, packages){	
	# Memory initialization
	if( !exists("state", where=car$mem) )
	{
		car$mem$state <- "EMPTY"
		car$mem$goal <- c()
		car$mem$packageCarried <- NA	
	}

	# Main state machine
	repeat
	{
		car$nextMove <- NA
		if( car$mem$state == "EMPTY" )
		{
			car$mem$packageCarried <- NA
			car$mem$goal <- NA
			start <- c(car$x, car$y)
			# Check if accidentaly ended up on a pickup point.
			# Do not determine the goal in that case.
			# Jump directly to MOVING_LOADED.
			for( i in 1:dim(packages)[1] )
			{
				if ( packages[i, 5] == 1 )
				{
					car$mem$packageCarried <- i
			        	car$mem$goal <- packages[i, c(3,4)]	
					# This is some stupid case. Just wait one turn.
					if( xy2Index(start) == xy2Index(car$mem$goal) )
					{
						car$nextMove <- 5
						car$mem$state <- "EMPTY"
					}
					else
						car$mem$state <- "MOVING_LOADED"	
					break;
				}
	
			}
			# Not accidentally ended up on a pickup up point.
			if( is.na(car$mem$packageCarried) )
			{
				# Do calculations here where to pick up the next parcel.
				# Initialize with high value.
				lowest_goal <- c(1, 1000)
				for( i in 1:dim(packages)[1] )
				{
					if( packages[i, 5] == 0 ) 
					{
						astar_res <- aStar(start, packages[i, c(1,2)], traffic$hroads, traffic$vroads)
						if( astar_res$cost < lowest_goal[2] )
						{
							# Better goal found -> it is closer..
							lowest_goal <- c(i, astar_res$cost)
						}
					}
				}
				# Settle goal
				car$mem$packageCarried <- lowest_goal[1]
				car$mem$goal <- packages[lowest_goal[1], c(1,2)]
				# This is some stupid case. Just wait one turn.
				if( xy2Index(start) == xy2Index(car$mem$goal) )
				{
					car$nextMove <- 5
					car$mem$state <- "EMPTY"
				}
				else
					car$mem$state <- "MOVING_EMPTY"	
			}
		}
		else if( car$mem$state == "MOVING_EMPTY" )	
		{
			# Move the car until goal reached
			if( xy2Index(c(car$x, car$y)) == xy2Index(car$mem$goal) )
			{
				car$mem$state <- "LOADED"
			}
			else
				car$nextMove <- aStar(c(car$x, car$y), car$mem$goal, traffic$hroads, traffic$vroads)$moves[1]	
		}
		else if( car$mem$state == "LOADED" )	
		{
			# Specify where parcel should go
			start <- c(car$x, car$y)
			car$mem$goal <- packages[car$mem$packageCarried, c(3,4)]
			car$mem$state <- "MOVING_LOADED"	
		}
		else if( car$mem$state == "MOVING_LOADED" )	
		{
			# Move the car until there are no more movements
			if( xy2Index(c(car$x, car$y)) == xy2Index(car$mem$goal) )
			{
				car$mem$state <- "EMPTY"
			}
			else	
				car$nextMove <- aStar(c(car$x, car$y), car$mem$goal, traffic$hroads, traffic$vroads)$moves[1] 
		}

		# Break only if a new next move is determined
		# Do not lose turns
		if( !is.na(car$nextMove) )
			break
	}
		return(car)
}
