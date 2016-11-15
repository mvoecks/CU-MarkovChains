#!/user/bin/Rscript

#PART 1: Simulating a homogeneous poisson point process

simulateHPP <- function(lambda, time){
    i <- 1
    uniform <- runif(1)
    arrivalTimes <- c(-log(uniform)/lambda)
    while (arrivalTimes[i] < time){
	uniform <- runif(1)
	arrivalTimes <- c(arrivalTimes, (arrivalTimes[i]-(log(uniform)/lambda)))
        i = i+1
    }
    return(arrivalTimes)
}

#PART 2: Simulating a non-homogeneous Poisson process (NHPP)

simulateNHPP <- function(time){
    C <- 26
    HPP <- simulateHPP(C, time)
    HPP <- HPP[1:length(HPP)-1]
    uniform <- runif(length(HPP))
    count <- 0
    for(i in 1:length(HPP)){
        if(uniform[[i]] < (((HPP[[i]]^2)-(10*HPP[[i]])+26)/C)){
            count = count+1
	}
    }
    return(count)
}

cat(rpois(10000, 12))

#10000 simulations of the HPP(3,4)
for(i in 1:10000){
    temp <- simulateHPP(3,4)
#    print(c(length(temp), temp[length(temp)-1], temp[length(temp)]))
    cat(',',length(temp))
}

#10000 simulations of the NHPP(t^2 - 10t + 26, 9)
#for(i in 1:10000){
#    cat(',',simulateNHPP(9))
#}
