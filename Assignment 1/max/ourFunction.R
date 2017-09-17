#############################################################################################
#
# Project:	Assignment 1, Artificial Intelligence, Uppsala University
# Author: 	Maximilian Stiefel
# Last mod.:	17.09.2017
#
#############################################################################################

#############################################################################################
#Libraries
#############################################################################################
library(DeliveryMan);

#############################################################################################
# Global variables
#############################################################################################
dim = 10

#############################################################################################
#
# Function to get a really easy heuristic estimation 
#
#############################################################################################
easyHeuristic <- function(node, goal)
{
	node <- index2XY(node)
	goal <- index2XY(goal)
	return( as.integer( abs(node[2]-goal[2]) + abs(node[1]-goal[1]) ) )
}

#############################################################################################
#
# Function to convert x and y to index
#
#############################################################################################
xy2Index <-function(node)
{
	x <- node[1]
	y <- node[2]
	return( (x-1)*dim + y )
}

#############################################################################################
#
# Function to convert an index to x and y
#
#############################################################################################
index2XY <- function(node)
{
	if( (node%%dim) == 0)
	{
		x <- node/dim
		y <- dim
	}
	else
	{
		x <- (as.integer(node/dim) + 1)
		y <- (node%%dim)
	}
	return( c(x,y) )
}

#############################################################################################
#
# Function to find the node which is not evaluated with the lowest fscore
#
#############################################################################################
findCurrent <- function(fscore, openSet)
{
	# Initialize with high value
	lowest_fscore <- c(1,1000)
	for(i in 1:(dim*dim))
	{
		# Is node used?
		if(openSet[i] == 1)
		{
			# Does it have a lower fscore than the lowest one found so far
			if(lowest_fscore[2] > fscore[i])
			{
				lowest_fscore[1] <- i
				lowest_fscore[2] <- fscore[i]
			}
		}
	}
	# Return an index
	return(lowest_fscore[1])
}

#############################################################################################
#
# Function to find all neighbors of a node considering, that the board has edges
#
#############################################################################################
findNeighbors <- function(node)
{
	node <- index2XY(node)
	x <- node[1]
	y <- node[2]
	# Add all possible neighbors
	neighbors <- list( c(x-1, y), c(x, y+1), c(x+1, y), c(x, y-1) )	
	# Throw out neighbors which do not exist
	i <- 1
	while(i <= length(neighbors))
	{		
		if( (neighbors[[i]][1] <= 0) || (neighbors[[i]][2] <= 0) || (neighbors[[i]][1] > dim) || (neighbors[[i]][2] > dim))
		{
			neighbors <- neighbors[-i]
			i <- i-1
		}	
		i <- i+1
	}
	# Convert all neighbors to indices		
	for(i in 1:length(neighbors))
	{
		neighbors[i] = xy2Index(neighbors[[i]])	
	}
	return(neighbors)
}

#############################################################################################
#
# Function to obtain the distance between two neighbors using vroads and hroads
#
#############################################################################################
calcDistance <- function(a, b, hroads, vroads)
{
	a <- index2XY(a)
	b <- index2XY(b)
	# Left neighbor
	if( (a[1]-1) == b[1])
		distance <- hroads[a[2], a[1]-1]
	# Right neighbor
	if( (a[1]+1) == b[1])
		distance <- hroads[a[2], a[1]]
	# Upper neighbor
	if( (a[2]-1) == b[2])
		distance <- vroads[a[2]-1, a[1]]	
	# Lower neighbor 
	if( (a[2]+1) == b[2])
		distance <- vroads[a[2], a[1]]
	return(distance)
}

#############################################################################################
#
# Function trace back the route to go giving a vector with necessary moves to go there 
#
#############################################################################################
traceBack <- function(start, goal, cameFrom)
{
	current <- goal
	moves <- c()
	i <- 1
	while(current != start)
	{
		# Go down
		if(cameFrom[current] == (current-1) )
		{
			moves[i] <- 8 
		}
		# Go left
		if(cameFrom[current] == (current+dim) )
		{
			moves[i] <- 4
		}
		# Go up
		if(cameFrom[current] == (current+1) )
		{
			moves[i] <- 2 
		}
		# Go right
		if(cameFrom[current] == (current-dim) )
		{
			moves[i] <- 6
		}
		current <- cameFrom[current]
		i <- i+1
	}
	# Turn vector arround
	return(rev(moves))
}

#############################################################################################
#
# Function using a lot of other functions to realize the A* algorithm
#
#############################################################################################
AStar <- function(start, goal, hroads, vroads)
{
	# Convert given coordinates to indices
	# Indices are the standard
	start <- xy2Index(start)
	goal <- xy2Index(goal)

	# For each node the cost to get from start to that node
	gscore <- matrix( c( rep(1000, dim*dim) ), nrow = dim, byrow = TRUE)
	gscore[start] <- 0	

	# For each node the cost to get from start to the goal (heuristic) 
	fscore <- matrix( c( rep(1000, dim*dim) ), nrow = dim, byrow = TRUE)
	# Initialize start with the heuristic 
	fscore[start] = easyHeuristic(start, goal);

	# Nodes which are already evaluated, 1 = evaluated, 0 = not evaluated
	closedSet <- matrix( c( rep(0, dim*dim) ), nrow = dim, byrow = TRUE)

	# Currently discovered nodes, 1 = discovered, 0 = not yet discovered
	openSet <- matrix( c(rep(0, dim*dim) ), nrow = dim, byrow = TRUE)
	# Initialize start node as only known node
	openSet[start] <- 1	

	# Matrix which is recording where the predecessor of each node is
	cameFrom <- matrix( c(rep(0, dim*dim) ), nrow = dim, byrow = TRUE)

	# Compare openSet with null matrix if identical there are no open nodes (we are done)
	while(!identical(openSet, matrix(c( rep(0, dim*dim) ), nrow = dim, byrow = TRUE ) ) )
	{	
		current <- findCurrent(fscore, openSet)
		if(current == goal)
		{
			return(traceBack(start, goal, cameFrom))
		}

		# Remove from openSet and add to closedSet
		openSet[current] = 0;	
		closedSet[current] = 1;
		for(neighbor in findNeighbors(current))
		{
			# Ignore neighbor if already evaluated
			if(closedSet[neighbor] == 1)
				next
			# If neighbor not discovered yet, discover it
			if(openSet[neighbor] != 1)
				openSet[neighbor] = 1
			# Calculate the new tentative gscore from current node to neighbor node
			tentative_gscore <- gscore[current] + calcDistance(current, neighbor, hroads, vroads)
			# Seems to be a bad node
			if(tentative_gscore > gscore[neighbor])
				next
			# Best node so far found. Record it.
			cameFrom[neighbor] <- current	
			gscore[neighbor] <- tentative_gscore
			fscore[neighbor] <- gscore[neighbor] + easyHeuristic(neighbor, goal) 	
		}	
	}
	return(FALSE)
}

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
	}
	
	# Main state machine
	car$nextMove <- NA
	repeat
	{
		if( car$mem$state == "EMPTY" )
		{
			# Do calculations here where to pick up the next parcel
			# This should be optimised for the final version to e.g. 
			# pick up the closest parcel
			start <- c(car$x, car$y)
			for(i in 1:dim(packages)[1])
			{
				if( packages[i, 5] == 0 )
				{
					goal <- packages[i, c(1,2)]
					car$mem$packageCarried <- i
					break
				}
			}
			cat("Pick up parcel at\n")
			print(start)
			print(goal)
			car$mem$moves<- AStar(start, goal, traffic$hroads, traffic$vroads)
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
			car$mem$moves <- AStar(start, goal, traffic$hroads, traffic$vroads) 
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
		readline(prompt="Press ENTER")

		# Break only if a new next move is determined
		# Do not lose turns
		if( !is.na(car$nextMove) )
			break
	}
		return(car)
}

# Run delivery man game
runDeliveryMan(carReady = ourFunction, dim = 10, turns = 1000, doPlot = T, pause = 0.1, del = 3);
