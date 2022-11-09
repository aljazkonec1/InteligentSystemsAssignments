library(tidyverse)
library(GA)
source("mazes.r")

m1 <- maze1

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
  # UKAZI:  1 - Levo
  #         2 - Desno
  #         3 - Gor
  #         4 - Dol
  

  for (i in 1: length(v_ukazi)) {
    switch( v_ukazi[i],
            pos[2] <- pos[2] - 1,
            pos[2] <- pos[2] + 1,
            pos[1] <- pos[1] - 1,
            pos[1] <- pos[1] + 1,
            pos <- pos
            )
    # treba dodati wall detection, da se ne premakne
    if( m1[pos] == finish){
      v_ukazi[i+1:] = 5
      return <- -score
    }
    if ( i == 1){
      score = score
    } else if (v_ukazi[i] == 1 & v_ukazi[i-1] == 2) {
      score = score + 1
    } else if ( v_ukazi[i] == 2 & v_ukazi[i-1] == 1) {
      score = score + 1
    } else if( v_ukazi[i] == 3 & v_ukazi[i-1] == 4) {
      score = score + 1
    } else if (v_ukazi[i] == 4 & v_ukazi[i-1] == 3) {
      score = score + 1
    }
    
  }
  print(sum(abs(finish - pos)))
  score = score + sum(abs(finish - pos))
  
  
  return <- score
  
}


s <- fitnes(c(2, 4, 2, 3, 3, 4, 1, 2, 3, 4, 1, 3, 3))
print(s)
