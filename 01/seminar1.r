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
  #v_ukazi = str_split(ukazi, "", simplify = T)
  pos <- start
  # print("na zacetku \n")
  # print(pos)
  # UKAZI:  0 - Levo
  #         1 - Desno # nolint # nolint
  #         2 - Gor
  #         3 - Dol
  #         4 - Stay
  
  #  if(v_ukazi[1] == 0 & m1[pos[1], pos[2] - 1] != "#" ){ # korak v levo
  #       pos[2] <- pos[2] - 1
  #   }
  #   else if(v_ukazi[1] == 1 & m1[pos[1], pos[2] + 1] != "#" ){  # korak v desno
  #       pos[2] <- pos[2] + 1
  #   }
  #    else if(v_ukazi[1] == 2 & m1[pos[1] - 1, pos[2]] != "#" ){  # korak gor
  #       pos[1] <- pos[1] - 1
  #   }
  #   else if(v_ukazi[1] == 3 & m1[pos[1] + 1, pos[2]] != "#" ){ # korak dol
  #       pos[1] <- pos[1] + 1
  #   } 
  #   else {
  #     score <- score +5
  #   }

  # for (i in 2: length(v_ukazi)) {
  
  #   v_ukazi = floor(v_ukazi)
    
  #   if(v_ukazi[i] == 0 & m1[pos[1], pos[2] - 1] != "#" &  v_ukazi[i-1] != 1 ){ # korak v levo
  #       pos[2] <- pos[2] - 1
  #   }
  #   else if(v_ukazi[i] == 1 & m1[pos[1], pos[2] + 1] != "#"  &  v_ukazi[i-1] != 0){  # korak v desno
  #       pos[2] <- pos[2] + 1
  #   }
  #    else if(v_ukazi[i] == 2 & m1[pos[1] - 1, pos[2]] != "#" &  v_ukazi[i-1] != 3){  # korak gor
  #       pos[1] <- pos[1] - 1
  #   }
  #   else if(v_ukazi[i] == 3 & m1[pos[1] + 1, pos[2]] != "#" &  v_ukazi[i-1] != 2){ # korak dol
  #       pos[1] <- pos[1] + 1
  #   } 
  #   else {
  #     score <- score +5
  #   }
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

    # if(v_ukazi[i] >= 0 && v_ukazi[i] < 1){ # korak v levo
    #   if(m1[pos[1], pos[2] - 1] != "#"){
    #     pos[2] <- pos[2] - 1
    #   }
    #   else{
    #     score <- score + 5 # nolint # nolint
    #   }
    # }
    
    # if(v_ukazi[i] >= 1 && v_ukazi[i] < 2){ # korak v desno
    #   if(m1[pos[1], pos[2] + 1] != "#"){
    #     pos[2] <- pos[2] + 1
    #   }
    #   else{
    #     score <- score + 5
    #   }
    # }
    
    # if(v_ukazi[i] >= 2 && v_ukazi[i] < 3){ # korak gor
    #   if(m1[pos[1] - 1, pos[2]] != "#"){
    #     pos[1] <- pos[1] - 1
    #   }
    #   else{
    #     score <- score + 5
    #   }
    # }
    
    # if(v_ukazi[i] >= 3 && v_ukazi[i] < 4){ # korak dol
    #   if(m1[pos[1] + 1, pos[2]] != "#"){
    #     pos[1] <- pos[1] + 1
    #   }
    #   else{
    #     score <- score + 5
    #   }
    # }
    
    # if(v_ukazi[i] >= 4 & v_ukazi[i] <= 5){ # stay
    #   pos <- pos
    # }
    
    # treba dodati wall detection, da se ne premakne
    # if(m1[pos] == "#"){
    #  v_ukazi[i+1:] = 5
    #  return <- -score
    #}


    # if (i == 1){ # kaznujemo backtrackanje, da spodbudimo premikanje
    #   score = score
    # } else if ((v_ukazi[i] >= 0 && v_ukazi[i] < 1) && (v_ukazi[i-1] >= 1 && v_ukazi[i-1] < 2)) {
    #   score = score + 3
    # } else if ((v_ukazi[i] >= 1 && v_ukazi[i] < 2) && (v_ukazi[i-1] >= 0 && v_ukazi[i-1] < 1)) {
    #   score = score + 3
    # } else if ((v_ukazi[i] >= 2 && v_ukazi[i] < 3) && (v_ukazi[i-1] >= 3 && v_ukazi[i-1] < 4)) {
    #   score = score + 3
    # } else if ((v_ukazi[i] >= 3 && v_ukazi[i] < 4) && (v_ukazi[i-1] >= 2 && v_ukazi[i-1] < 3)) {
    #   score = score + 3
    # }

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
    print( length(v))
  }

  score = score + sum(abs(finish - pos))
  
  
  return(-score)
  
}

postf <- function(o) {
  pop <- o@population
  
    for (i in 1:length(pop[,1])) {
      pop[1,] <- append(pop[1,], runif(1, 0, 4))
    }  

  o@population <- pop
}    


# v <- c(  1   ,1,   1,   1,   1,   1,   1,   0,   0 ,  0,   0 ,  1,   2,   0 ,  0,   0,  0, 2 , 1 ,  1,2 ,  1 ,  1,   1  , 1 , 1   ,3 ,  3  , 0 , 0  , 0  , 0  , 0 ,  1 ,  1 ,  1,  1 ,  2  , 0 ,  1, 1 ,  2  , 2  , 0  , 1  ,3  , 1 , 3  , 0  , 0  , 0 ,  0 ,  0  , 2 ,  0  , 0 , 2  , 1,   1 ,  1 , 1  , 1  , 1   ,1)
# v <- c(2,0,3,0,2,2,0,2,2,1,1,1,2,2,2,2,2,1,1,2,2,1,1,1,1,1,1,2,2,1)

# fitnes(v)



{
nSteps <- (length(m[1,])-2)*(length(m[,1])-2)
nSteps <- 33 ## več kot število pik korakov nemore naredit
lBound <- rep(0, nSteps)
lBound
# uBound <- rep(4, nSteps)
lBound <- c(0)
uBound <- c(4)

}

GA <- ga(type = "real-valued", fitness = fitnes, lower = rep(0, 1), upper = rep(4,1), maxiter = 500, popSize = 1000, pmutation = 0.2, postFitness = postf)

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

