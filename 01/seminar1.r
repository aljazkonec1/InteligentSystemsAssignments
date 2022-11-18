library(tidyverse)
library(GA)
source("01\\mazes.r")


m <- maze3_T

m <- str_split(m, "", Inf, simplify = TRUE)
m
start <- which(m == "S", arr.ind = TRUE)
finish <- which(m == "E", arr.ind = TRUE)



fitnes <- function(v_ukazi) { # nolint
  score <- 0
  pos <- start
 
  v_ukazi <- floor(v_ukazi)
  m1 <- m
  for (i in 1: length(v_ukazi)) {
  
    
    m1[pos] = "#"

    if(v_ukazi[i] == 0 & m1[pos[1], pos[2] - 1] != "#" ){ # korak v levo
        pos[2] <- pos[2] - 1
    }
    else if(v_ukazi[i] == 1 & m1[pos[1], pos[2] + 1] != "#"){  # korak v desno
        pos[2] <- pos[2] + 1
    }
     else if(v_ukazi[i] == 2 & m1[pos[1] - 1, pos[2]] != "#"){  # korak gor
        pos[1] <- pos[1] - 1
    }
    else if(v_ukazi[i] == 3 & m1[pos[1] + 1, pos[2]] != "#"){ # korak dol
        pos[1] <- pos[1] + 1
    } 
    else {
      return( score - (sum(abs(finish - pos))/(score/length(m[1,]) + 1))  * nSteps + (sum(abs(start - pos))/ sum(abs(finish - pos))) * nSteps )
    }

    score <- score + length(m[1,])
   

    if(m1[pos] == "E"){
      return(Inf)
    }

    
  }
  
  return(score+ (sum(abs(start - pos))/ sum(abs(finish - pos))) * nSteps)
  
}




{
nSteps <- sum(m == ".") +1
lBound <- rep(0, nSteps)
uBound <- rep(4, nSteps)

}

GA <- ga(type = "real-valued", fitness = fitnes, lower = lBound, upper = uBound, maxiter= 1000, popSize = 2000, pmutation = 1, pcrossover = 0.5 )
printPoteze(floor(GA@solution[1, ]))

plot(GA)

print(floor(GA@solution[1, ]))




printPoteze <- function(x) {
  r <- c()
  m1 <- m
  pos <- start
  for ( i in 1:nSteps) {
    if(x[i] == 0 & m1[pos[1], pos[2] - 1] != "#" ){ # korak v levo
        r <- c(r, "L")
        pos[2] <- pos[2] - 1
        # m1[pos] <- i
    }
    else if(x[i] == 1 & m1[pos[1], pos[2] + 1] != "#"){  # korak v desno
        r <- c(r, "R")
        pos[2] <- pos[2] + 1
        # m1[pos] <- i
    }
     else if(x[i] == 2 & m1[pos[1] - 1, pos[2]] != "#"){  # korak gor
        r <- c(r, "U")
        pos[1] <- pos[1] - 1
        # m1[pos] <- i
    }
    else if(x[i] == 3 & m1[pos[1] + 1, pos[2]] != "#"){ # korak dol
        r <- c(r, "D")
        pos[1] <- pos[1] + 1
        # m1[pos] <- i
    } 

    if ( m[pos] == "E") {
      # print(m1)
      # print(r)
      return(r)
    }
  }
  # m1[pos] <- "X"
  # print(m1)
  return(r)

}

manhatan <- function(a, b) {
  sum(abs(a - b))
}


origMaze <- m
m 
t <- which(m== "T", arr.ind = TRUE)
len <- c()
t
for ( i in 1:length(t[,1])){
  len <- c(len,  manhatan(t[i,], start))

}
t <- cbind(t, len)
t <- t[order(t[, 3]), ]
t

t <- rbind(c(start, 0), t)
t <- rbind(t, c(finish, 0))
t <- t[, 1:2]
t

m[t] <- "."
m 

v <- c()

for (i in 2:length(t[,1])){
  m[t[i-1, ]] <- "S"
  m[t[i,] ]<- "E"
  nSteps <- sum(m == ".") +1
  lBound <- rep(0, nSteps)
  uBound <- rep(4, nSteps)
  GA <- ga(type = "real-valued", fitness = fitnes, lower = lBound, upper = uBound, maxiter= 1000, popSize = 2000, pmutation = 1, pcrossover = 0.5 )
  v <- c(v,printPoteze(floor(GA@solution[1, ])) )
  m[t] <- "."

}

v



