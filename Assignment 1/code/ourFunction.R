#############################################################################################
#
# Project:	Assignment 1, Artificial Intelligence, Uppsala University
# Author: 	Maximilian Stiefel
# Last mod.:	18.09.2017
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
		car$mem$moves <- c()
		car$mem$packageCarried <- NA
		car$mem$vroads <- list()
		car$mem$hroads <- list()
	}

	# Main state machine
	car$nextMove <- NA
	repeat
	{
		if( car$mem$state == "EMPTY" )
		{
			# Do calculations here where to pick up the next parcel
			start <- c(car$x, car$y)
			# Initialize with high value
			lowest_goal <- c(1, 1000)
			for( i in 1:dim(packages)[1] )
			{
				if( packages[i, 5] == 0 ) 
				{
					astar_res <- AStar(start, packages[i, c(1,2)], traffic$hroads, traffic$vroads)
					if( astar_res$cost < lowest_goal[2] )
					{
						# Better goal found -> it is closer
						lowest_goal <- c(i, astar_res$cost)
					}
				}
			}
			#Settle goal
			car$mem$packageCarried <- lowest_goal[1]
			goal <- packages[lowest_goal[1], c(1,2)]
			cat("Pick up parcel at\n")
			print(start)
			print(goal)
			car$mem$moves <- astar_res$moves
			car$mem$state <- "MOVING_EMPTY"	
		}
		else if( car$mem$state == "MOVING_EMPTY" )	
		{
			# Move the car until there are no more movements 
			car$nextMove <- car$mem$moves[1]
			car$mem$moves <- car$mem$moves[-1]
			if( length(car$mem$moves) == 0 )
				car$mem$state <- "LOADED"
		}
		else if( car$mem$ state == "LOADED" )	
		{
			# Do calculations where to deliver parcel
			start <- c(car$x, car$y)
			goal <- packages[car$mem$packageCarried, c(3,4)]
			car$mem$moves <- AStar(start, goal, traffic$hroads, traffic$vroads)$moves 
			car$mem$state <- "MOVING_LOADED"	
		}
		else if( car$mem$ state == "MOVING_LOADED" )	
		{
			# Move the car until there are no more movements
			car$nextMove <- car$mem$moves[1]
                        car$mem$moves <- car$mem$moves[-1]
                        if( length(car$mem$moves) == 0 )
				car$mem$state <- "EMPTY"	
		}
	
		cat("State: ")
		print(car$mem$state)
		cat("Next move: ")
		print(car$nextMove)
		#readline(prompt="Press ENTER")

		# Break only if a new next move is determined
		# Do not lose turns
		if( !is.na(car$nextMove) )
			break
	}
		return(car)
}

# Run delivery man game
runDeliveryMan(carReady = ourFunction, dim = 10, turns = 1000, doPlot = T, pause = 0.1, del = 5);
