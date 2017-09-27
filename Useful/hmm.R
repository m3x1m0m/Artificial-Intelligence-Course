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
