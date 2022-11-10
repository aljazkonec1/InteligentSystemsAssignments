library(tidyverse)
library(GA)
source("mazes.r")

m1 <- c("##E##",
        "#...#",
        "#...#",
        "#S..#",
        "#####")#maze1

print(m1)

m1 <- str_split(m1, "", Inf, simplify = TRUE)
print(m1)
start <- which(m1 == "S", arr.ind = T)
finish <- which(m1 == "E", arr.ind = T)


fitnes <- function(v_ukazi) 
{
  score <- 0
  #v_ukazi = str_split(ukazi, "", simplify = T)
  pos <- start
  # UKAZI:  0->1 - Levo
  #         1->2 - Desno
  #         2->3 - Gor
  #         3->4 - Dol
  #         4->5 - Stay
  
  
  for (i in 1: length(v_ukazi)) {
    #switch( v_ukazi[i],
    #        pos[2] <- pos[2] - 1,
    #        pos[2] <- pos[2] + 1,
    #        pos[1] <- pos[1] - 1,
    #        pos[1] <- pos[1] + 1,
    #        pos <- pos
    #)
    
    if(v_ukazi[i] >= 0 & v_ukazi[i] < 1){ # korak v levo
      pos[2] <- pos[2] - 1
    }
    
    if(v_ukazi[i] >= 1 & v_ukazi[i] < 2){ # korak v desno
      pos[2] <- pos[2] + 1
    }
    
    if(v_ukazi[i] >= 2 & v_ukazi[i] < 3){ # korak gor
      pos[2] <- pos[2] - 1
    }
    
    if(v_ukazi[i] >= 3 & v_ukazi[i] < 4){ # korak dol
      pos[2] <- pos[2] + 1
    }
    
    if(v_ukazi[i] >= 4 & v_ukazi[i] <= 5){ # stay
      pos <- pos
    }
    
    # treba dodati wall detection, da se ne premakne
    #if( m1[pos] == finish){
    #  v_ukazi[i+1:] = 5
    #  return <- -score
    #}
    if ( i == 1){ # kaznujemo backtrackanje, da spodbudimo premikanje
      score = score
    } else if ((v_ukazi[i] >= 0 & v_ukazi[i] < 1) & (v_ukazi[i-1] >= 1 & v_ukazi[i-1] < 2)) {
      score = score + 1
    } else if ((v_ukazi[i] >= 1 & v_ukazi[i] < 2) & (v_ukazi[i-1] >= 0 & v_ukazi[i-1] < 1)) {
      score = score + 1
    } else if ((v_ukazi[i] >= 2 & v_ukazi[i] < 3) & (v_ukazi[i-1] >= 3 & v_ukazi[i-1] < 4)) {
      score = score + 1
    } else if ((v_ukazi[i] >= 3 & v_ukazi[i] < 4) & (v_ukazi[i-1] >= 2 & v_ukazi[i-1] < 3)) {
      score = score + 1
    }
    
  }
  print(sum(abs(finish - pos)))
  score = score + sum(abs(finish - pos))
  
  
  return <- -score
  
}

nSteps <- (length(m1)*length(m1[1]))
lBound <- rep(0, nSteps)
uBound <- rep(5, nSteps)


GA <- ga(type = "real-valued", fitness = fitnes, lower = lBound, upper = uBound)

plot(GA)
summary(GA)
print(GA@solution)


#s <- fitnes(c(2, 4, 2, 3, 3, 4, 1, 2, 3, 4, 1, 3, 3))
#print(s)
