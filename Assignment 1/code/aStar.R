#############################################################################################
#
# Project:	Assignment 1, Artificial Intelligence, Uppsala University
# Author: 	Maximilian Stiefel
# Last mod.:	18.09.2017
# File:		aStar.R
# Description:	A* algorithm
#
#############################################################################################

#############################################################################################
#Libraries
#############################################################################################
library(DeliveryMan)

#############################################################################################
#Source files
#############################################################################################
source('hFunctions.R')

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
			# Return moves and costs
			return( list(moves=traceBack(start, goal, cameFrom), cost=fscore[current]) )
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
