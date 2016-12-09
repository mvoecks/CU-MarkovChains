#!/user/bin/Rscript
#5000 1839.397 676.6764 248.9353 91.57819 33.68973 12.39376 4.55941 1.677313
#Simulating a M/M/1 queue:
simulateMM1 <- function(lambda, mu, n, endTime){
    t <- 0
    busyTime <- 50
    nextT <- 0
    u <- 0
    interDeparture <- 0
    interDeparturetimes <- c()
    repeat{
        if(n == 0){
            nextT <- rexp(1, lambda)
            t <- t+nextT
	    n <- n+1
	    busyTime <- busyTime-nextT
        }
        else{
            nextT <- rexp(1, lambda+mu)
	    t <- t+nextT
	    u <- runif(1)
	    if(u <= (lambda/(lambda+mu))){
                n <- n+1
	    }
	    else{
                n <- n-1
	        interDeparturetimes <- c(interDeparturetimes, t-interDeparture)
		interDeparture <- t
	    }
	}
	if(t >= endTime){
	    break
	}
    }
    return(interDeparturetimes)
}

#temp <- c()
#for (i in 1:1000){
#    temp <- c(temp, simulateMM1(1,2,rgeom(1,.5),50))
#}

#cat(temp)
cat(dexp(0:8, 1)*5000)	
