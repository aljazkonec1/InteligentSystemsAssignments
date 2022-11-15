library(tidyverse)
library(GA)
source("mazes.r")


m <- maze3


m <- str_split(m, "", Inf, simplify = TRUE)
m
start <- which(m == "S", arr.ind = TRUE)
finish <- which(m == "E", arr.ind = TRUE)


fitnes <- function(v_ukazi) { # nolint
  score <- 0
  pos <- start
 
  m1 <- m
  for (i in 1: length(v_ukazi)) {
  
    v_ukazi = floor(v_ukazi)
    
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
      score <- score +10
    }

   

    if(m1[pos] == "E"){
      print("na koncu")
      # print(v_ukazi)
      return(-score)
    }

    # score <- score + 1
    
    # print("Mesto: ")
    # print(i)
    # print(" Score: ")
    # print(score)
    # print("pos: ")
    # print(pos)
  }
    print( length(v_ukazi))
  
  score = score + sum(abs(finish - pos))
  
  
  return(-score)
  
}

postf <- function(o) {
  pop <- o@population
  o@population <- cbind(pop, runif(o@popSize, 0, 4))
  o@lower <- c(o@lower, 0)
  o@upper <- c(o@upper, 4)

}    




{
nSteps <- (length(m[1,])-2)*(length(m[,1])-2)
nSteps <- 1 ## več kot število pik korakov nemore naredit
lBound <- rep(0, nSteps)
uBound <- rep(4, nSteps)
# lBound <- c(0)
# uBound <- c(4)

}

GA <- ga(type = "real-valued", fitness = fitnes, lower = lBound, upper = uBound, maxiter= 500, popSize = 10, pmutation = 0.2, postFitness= postf )

plot(GA)

GA@solution[1,]
# resitev = floor(GA@solution[1,])
resitev = GA@solution[1,]
resitev
print(floor(GA@solution[1, ]))


for (i in 1: length(resitev)) {
    #switch( v_ukazi[i],
    #        pos[2] <- pos[2] - 1,
    #        pos[2] <- pos[2] + 1,
    #        pos[1] <- pos[1] - 1,
    #        pos[1] <- pos[1] + 1,
    #        pos <- pos
    #)
    
    if(resitev[i] >= 0 && resitev[i] < 1){ # korak v levo
      print("L ")
    }
    
    if(resitev[i] >= 1 && resitev[i] < 2){ # korak v desno
       print("R ")
    }
    
    if(resitev[i] >= 2 && resitev[i] < 3){ # korak gor
      print("U ")
    }
    
    if(resitev[i] >= 3 && resitev[i] < 4){ # korak dol
      print("D ")
    }

    
}

