library(tidyverse)
library(GA)
source("mazes.r")


m <- maze6

m <- str_split(m, "", Inf, simplify = TRUE)
m
start <- which(m == "S", arr.ind = TRUE)
finish <- which(m == "E", arr.ind = TRUE)


fitnes <- function(v_ukazi) { # nolint
  score <- 0
  pos <- start
 
    v_ukazi <- floor(v_ukazi)
  # print(v_ukazi)
  m1 <- m
  for (i in 1: length(v_ukazi)) {
  
    
    # m1[pos] = "#"

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
      return( - (score +1)/nSteps + (sum(abs(start - pos))/ sum(abs(finish - pos))) * 100)
      # - nSteps 
    }

   

    if(m1[pos] == "E"){
      print("na koncu")
      # print(v_ukazi)
      return(Inf)
    }

    score <- score + 1
    
    # print("Mesto: ")
    # print(i)
    # print(" Score: ")
    # print(score)
    # print("pos: ")
    # print(pos)
  }
  
  return(- (score +1)/nSteps + (sum(abs(start - pos))/ sum(abs(finish - pos))) * 10)
  
}




{
# nSteps <- (length(m[1,])-2)*(length(m[,1])-2)
# nSteps <- length(m[1,]) * length(m[,1])
# nSteps <- 1 ## več kot število pik korakov nemore naredit
nSteps <- sum(m == ".") +1

lBound <- rep(0, nSteps)
uBound <- rep(4, nSteps)
# lBound <- c(0)
# uBound <- c(4)

}

GA <- ga(type = "real-valued", fitness = fitnes, lower = lBound, upper = uBound, maxiter= 10000, popSize = 2000, pmutation = 1, pcrossover = 0.5 )
printPoteze(floor(GA@solution[1, ]))

plot(GA)

print(floor(GA@solution[1, ]))



# spremeni da gre print samo do vrednsti BEST || Finish!!!

printPoteze <- function(x) {
  r <- ""
  m1 <- m
  pos <- start
  for ( i in 1:nSteps) {
    if(x[i] == 0 & m1[pos[1], pos[2] - 1] != "#" ){ # korak v levo
        r <- c(r, "L")
        pos[2] <- pos[2] - 1
        m1[pos] <- i
    }
    else if(x[i] == 1 & m1[pos[1], pos[2] + 1] != "#"){  # korak v desno
        r <- c(r, "R")
        pos[2] <- pos[2] + 1
        m1[pos] <- i
    }
     else if(x[i] == 2 & m1[pos[1] - 1, pos[2]] != "#"){  # korak gor
        r <- c(r, "U")
        pos[1] <- pos[1] - 1
        m1[pos] <- i
    }
    else if(x[i] == 3 & m1[pos[1] + 1, pos[2]] != "#"){ # korak dol
        r <- c(r, "D")
        pos[1] <- pos[1] + 1
        m1[pos] <- i
    } 

    if ( m[pos] == "E") {
      print(m1)
      print(r)
      return(r)
    }
  }
  m1[pos] <- "X"
  print(m1)
  print(r)
}


