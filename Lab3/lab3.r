#!/user/bin/Rscript
#5000 1839.397 676.6764 248.9353 91.57819 33.68973 12.39376 4.55941 1.677313
#5000 2500 1250 625 312.5 156.25 78.125 39.0625 19.53125
#
#Simulating a M/M/1 queue:
simulateMM1 <- function(lambda, mu, n, endTime){
    t <- 0
    busyTime <- 50
    nextT <- 0
    u <- 0
    interDeparture <- 0
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
		interDeparture <- interDeparture+1
	    }
	}
	if(t >= endTime){
	    break
	}
    }
    return(interDeparture)
}

#temp <- c()
#for (i in 1:10000){
#    temp <- c(temp, simulateMM1(1,2,rgeom(1,.5),50))
#}

cat(dpois(30:80, 50)*10000)
