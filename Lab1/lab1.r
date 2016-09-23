#!/usr/bin/Rscript

#SECTION 1

#part 1
#This function accepts an integer n and returns a random permutation of the sequence (1, 2, 3, 4, ... , n-1, n)
permutationCreator <- function(n){
  UniformList <- runif(n,0,1);
  UniformSortedList <- sort(UniformList);
  permutation <- NULL;
  for(i in 1:n){
    permutation <-c(permutation, match(UniformSortedList[[i]], UniformList))
  }
  return(permutation);
}

sigma <- c(6,7,2,5,1,4,3)
#part 2
createX <- function(){
  permutation <- NULL
  count <- 0
  for(i in 1:6000){
    permutation = permutationCreator(7)
    if(all(permutation == sigma)){
      count <- count+1
    }
  }
  return(count)
}

#part 3
createY <- function(){
  count <- 0
  stop <- 0
  while(stop != 1){
    count <- count+1
    permutation = permutationCreator(7)
    if(all(permutation == sigma)){
      stop <- 1
    }
  }
  return(count)
}


if(FALSE){
#part 4
countArrayX <- NULL
countArrayY <- NULL
for(j in 1:2000){
  countArrayX <- c(countArrayX, createX())
}
for(j in 1:2000){
  countArrayY <- c(countArrayY, createY())
}

# histogram X
png("histogramX.png")
barplot(as.vector(table(countArrayX)))
dev.off()

# histogram Y
png("histogramY.png")
hist(countArrayY);
dev.off()

# Binomial barplot
png("binomialdistribution.png")
barplot(dbinom(0:10, 6000, 1/5040)*2000)
dev.off()
}


simulateQa <- function(n){
  rPermutation <- NULL
  count <- 0
  countQa <- NULL
  for(i in 1:10000){
    rPermutation = permutationCreator(n);
    count <- 0
    for(i in 1:n){
      count <- count+1
      if(rPermutation[[i]] == 1)
        break
    }
    countQa <- c(countQa, count)
  }
  return(countQa)
}


average <- mean(simulateQa(9))
print(average)
average <- mean(simulateQa(21))
print(average)
average <- mean(simulateQa(36))
print(average)
average <- mean(simulateQa(69))
print(average)

