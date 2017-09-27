mostlikelyTrack <- function(PM, TM, EM, obs)
{
	for(i in i:length(obs))
	{
		P1 <- PM[i,1]*	
	}	
}

#############################################################################################
#
# Function to use the forward algorithm to do state estimation
#
#############################################################################################
calcProbabilities <- function(FM, BM)
{
	PM <- matrix(rep(0,length(FM)), ncol=2)
	for(i in 1:dim(FM)[1])
	{
		PM[i,1] <- FM[i,1]*BM[i,1]
		PM[i,2] <- FM[i,2]*BM[i,2]
		A <- PM[i,1] + PM[i,2] 
		PM[i, ] <- PM[i, ]/A
	}
	return(PM)	
}

#############################################################################################
#
# Function to use the backward algorithm to give a smoothed distribution
#
#############################################################################################
smoothing <- function(FM, TM, EM, obs)
{
	BM <- matrix( rep( 0,2*(length(obs)+1) ), ncol=2)
        BM[length(obs)+1,] <- c(1,1)
	for(i in length(obs):1)
	{
		for(j in 1:2)
		{
			BM[i,1] <- BM[i,1] + TM[1,j]*BM[i+1,j]*EM[j,obs[i]]	
		}
		for(j in 1:2)
		{
			BM[i,2] <- BM[i,2] + TM[2,j]*BM[i+1,j]*EM[j,obs[i]]	
		}
	}
	return (BM)
}

#############################################################################################
#
# Function to predict state that lies x steps in the future (Markov Chain)
#
#############################################################################################
predictState <- function(ST, TM, x)
{
	return(ST %*% TM^x)
}

#############################################################################################
#
# Function to use the forward algorithm to do state estimation
#
#############################################################################################
stateEstimation <- function(FMinit, TM, EM, obs)
{
	FM <- matrix( rep( 0,2*(length(obs)+1) ), ncol=2)
	FM[1,] <- FMinit
	for(i in 1:length(obs))
	{
		# Generate sth. like this
		# e1x 	0
		# 0	e2x
		EX <- matrix(rep(0, 4), nrow=2, byrow=T)
		EX[1] <- EM[1,obs[i]]
		EX[4] <- EM[2,obs[i]]
		# Calculate new probabilities
		FM[i+1, ] <- FM[i, ] %*% TM %*% EX
		# Normalization
		A <- FM[i+1,1] + FM[i+1,2]
		FM[i+1, ] <- FM[i+1, ]/A	
	}
	return(FM)
}

TM <- matrix(c(0.8, 0.2, 0.25, 0.75), nrow=2, byrow=T)
EM <- matrix(c(0.99, 0.01, 0.5, 0.5), nrow=2, byrow=T)
# Observed states
obs <- c(2, 2, 1, 2, 1)
FMinit <- c(1, 0)
FM <- stateEstimation(FMinit, TM, EM, obs)
print(FM)
print(predictState(FM[6,],TM,1))
BM <- smoothing(FM, TM, EM, obs)
print(BM)
PM <- calcProbabilities(FM, BM)
print(PM)
