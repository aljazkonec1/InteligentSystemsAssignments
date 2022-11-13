library(tidyverse)
library(GA)
source("mazes.r")

# m1 <- c("##E##",
#         "#...#",
#         "#...#",
#         "#S..#",
#         "#####")#maze1

{
m1 <- maze3

print(m1)

m1 <- str_split(m1, "", Inf, simplify = TRUE)
m1
start <- which(m1 == "S", arr.ind = TRUE)
finish <- which(m1 == "E", arr.ind = TRUE)
}


fitnes <- function(v_ukazi, maze) {
  maze
  score <- 0
  #v_ukazi = str_split(ukazi, "", simplify = T)
  pos <- start
  # print("na zacetku \n")
  # print(pos)
  # UKAZI:  0->1 - Levo
  #         1->2 - Desno
  #         2->3 - Gor
  #         3->4 - Dol
  #         4->5 - Stay
  
  #print(v_ukazi)
  
  for (i in 1: length(v_ukazi)) {
    #switch( v_ukazi[i],
    #        pos[2] <- pos[2] - 1,
    #        pos[2] <- pos[2] + 1,
    #        pos[1] <- pos[1] - 1,
    #        pos[1] <- pos[1] + 1,
    #        pos <- pos
    #)
    
    if(v_ukazi[i] >= 0 && v_ukazi[i] < 1){ # korak v levo
      if(m1[pos[1], pos[2] - 1] != "#"){
        pos[2] <- pos[2] - 1
      }
      else{
        score <- score + 5
      }
    }
    
    if(v_ukazi[i] >= 1 && v_ukazi[i] < 2){ # korak v desno
      if(m1[pos[1], pos[2] + 1] != "#"){
        pos[2] <- pos[2] + 1
      }
      else{
        score <- score + 5
      }
    }
    
    if(v_ukazi[i] >= 2 && v_ukazi[i] < 3){ # korak gor
      if(m1[pos[1] - 1, pos[2]] != "#"){
        pos[1] <- pos[1] - 1
      }
      else{
        score <- score + 5
      }
    }
    
    if(v_ukazi[i] >= 3 && v_ukazi[i] < 4){ # korak dol
      if(m1[pos[1] + 1, pos[2]] != "#"){
        pos[1] <- pos[1] + 1
      }
      else{
        score <- score + 5
      }
    }
    
    # if(v_ukazi[i] >= 4 & v_ukazi[i] <= 5){ # stay
    #   pos <- pos
    # }
    
    # treba dodati wall detection, da se ne premakne
    # if(m1[pos] == "#"){
    #  v_ukazi[i+1:] = 5
    #  return <- -score
    #}
    if ( i == 1){ # kaznujemo backtrackanje, da spodbudimo premikanje
      score = score
    } else if ((v_ukazi[i] >= 0 && v_ukazi[i] < 1) && (v_ukazi[i-1] >= 1 && v_ukazi[i-1] < 2)) {
      score = score + 4
    } else if ((v_ukazi[i] >= 1 && v_ukazi[i] < 2) && (v_ukazi[i-1] >= 0 && v_ukazi[i-1] < 1)) {
      score = score + 4
    } else if ((v_ukazi[i] >= 2 && v_ukazi[i] < 3) && (v_ukazi[i-1] >= 3 && v_ukazi[i-1] < 4)) {
      score = score + 4
    } else if ((v_ukazi[i] >= 3 && v_ukazi[i] < 4) && (v_ukazi[i-1] >= 2 && v_ukazi[i-1] < 3)) {
      score = score + 4
    }

    if(m1[pos[1],pos[2]] == "E"){
      return(-score)
    }

    score <- score + 1
    
  }
  # print(sum(abs(finish - pos))) # oddaljenost od konca
  # print( pos)
  score = score + sum(abs(finish - pos))
  
  
  return(-score)
  
}

{
nSteps <- (length(m1[1,])-2)*(length(m1[,1])-2)
print(nSteps)
lBound <- rep(0, nSteps)
uBound <- rep(4, nSteps)
}

GA <- ga(type = "real-valued", fitness = fitnes, lower = lBound, upper = uBound, maxiter = 300)
plot(GA)

resitev = floor(GA@solution[1,])

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
