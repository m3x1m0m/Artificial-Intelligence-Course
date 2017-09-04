library(DeliveryMan);

# Our function
ourFunction <- function(traffic, car, packages){
	print(traffic)	
	print(car)
	print(packages)
	return (car)
}

runDeliveryMan(carReady = ourFunction, dim = 10, turns = 2000, doPlot = T, pause = 0.1, del = 5);
