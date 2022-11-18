library(tidyverse)
library(GA)
source("mazes.r")

{
m1 <- maze6
m1 <- str_split(m1, "", Inf, simplify = TRUE)
m1
start <- which(m1 == "S", arr.ind = TRUE)
finish <- which(m1 == "E", arr.ind = TRUE)
}

fitnes <- function(v_ukazi) {
  maze <- maze6
  maze <- str_split(maze, "", Inf, simplify = TRUE)
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
      # else{
      #   score <- score + 3
      # }
    }
    
    if(v_ukazi[i] >= 1 && v_ukazi[i] < 2){ # korak v desno
      if(m1[pos[1], pos[2] + 1] != "#"){
        pos[2] <- pos[2] + 1
      }
      # else{
      #   score <- score + 3
      # }
    }
    
    if(v_ukazi[i] >= 2 && v_ukazi[i] < 3){ # korak gor
      if(m1[pos[1] - 1, pos[2]] != "#"){
        pos[1] <- pos[1] - 1
      }
      # else{
      #   score <- score + 3
      # }
    }
    
    if(v_ukazi[i] >= 3 && v_ukazi[i] < 4){ # korak dol
      if(m1[pos[1] + 1, pos[2]] != "#"){
        pos[1] <- pos[1] + 1
      }
      # else{
      #   score <- score + 3
      # }
    }
    
    if(v_ukazi[i] >= 4 && v_ukazi[i] <= 5){ # stay
      score <- score - 1
      pos <- pos
    }
    
    # treba dodati wall detection, da se ne premakne
    # if(m1[pos] == "#"){
    #  v_ukazi[i+1:] = 5
    #  return <- -score
    #}
    
    if(maze[pos[1],pos[2]] == 1){
      score <- score + 3
    } else {
      maze[pos[1],pos[2]] <- 1
    }

    score <- score + 1

    if(m1[pos[1],pos[2]] == "E"){
      return(-score)

    }

    
  }


  # print(sum(abs(finish - pos))) # oddaljenost od konca
   
  score = score + sum(abs(finish - pos))

  return(-score)
  
}

{count <- 1
for(i in 1:length(m1[1,])){
  for(j in 1:length(m1[,1])){
    if(m1[i,j] == "."){
      count <- count + 1
    }
  }
}
print(count)
lBound <- rep(0, count)
uBound <- rep(4.2, count)
}

resitevPrint <- function(resitev){
  a = (-1)*fitnes(resitev)
  b = length(resitev)

  if(a < b){
    c = a
  } else 
  {
    c = b
  }

  for (i in 1:c) {
    #switch( v_ukazi[i],
    #        pos[2] <- pos[2] - 1,
    #        pos[2] <- pos[2] + 1,
    #        pos[1] <- pos[1] - 1,
    #        pos[1] <- pos[1] + 1,
    #        pos <- pos
    #)    
    if(resitev[i] >= 0 && resitev[i] < 1){ # korak v levo
      print("L")
    }
    
    if(resitev[i] >= 1 && resitev[i] < 2){ # korak v desno
       print("R")
    }
    
    if(resitev[i] >= 2 && resitev[i] < 3){ # korak gor
      print("U")
    }
    
    if(resitev[i] >= 3 && resitev[i] < 4){ # korak dol
      print("D")
    }

    if(resitev[i] >= 4 && resitev[i] < 5){ # korak dol
      #print("S")
      c <- c+1
    }

  }
}

myMutation <- function(object, parent){
#Select the parent vector from the population
m1 <- maze6
m1 <- str_split(m1, "", Inf, simplify = TRUE)
mutate <- parent <- as.vector(object@population[parent,])
n <- length(parent)
#Sample a random vector element that should be changed
j <- sample(1:n, size = 1)
pos <- start
for (i in 1:j) {
    #switch( v_ukazi[i],
    #        pos[2] <- pos[2] - 1,
    #        pos[2] <- pos[2] + 1,
    #        pos[1] <- pos[1] - 1,
    #        pos[1] <- pos[1] + 1,
    #        pos <- pos
    #)
    
    if(parent[i] >= 0 && parent[i] < 1){ # korak v levo
      if(m1[pos[1], pos[2] - 1] != "#"){
        pos[2] <- pos[2] - 1
      }
    }
    
    if(parent[i] >= 1 && parent[i] < 2){ # korak v desno
      if(m1[pos[1], pos[2] + 1] != "#"){
        pos[2] <- pos[2] + 1
      }
    }
    
    if(parent[i] >= 2 && parent[i] < 3){ # korak gor
      if(m1[pos[1] - 1, pos[2]] != "#"){
        pos[1] <- pos[1] - 1
      }
    }
    
    if(parent[i] >= 3 && parent[i] < 4){ # korak dol
      if(m1[pos[1] + 1, pos[2]] != "#"){
        pos[1] <- pos[1] + 1
      }
    }

    if(m1[pos[1], pos[2]] == "E"){
      mutate[j] <- 5
      return(mutate)
    }
  }

if(m1[pos[1], pos[2]] == "E"){
      mutate[j] <- 5
      return(mutate)
    }
#Preveri, v katere strani se lahko mutira v tej tocki, da ne gre v steno

validDirections = c(0,0,0,0,1)

if(m1[pos[1], pos[2] - 1] != "#"){ #Levo
  validDirections[1] = 1
}
if(m1[pos[1], pos[2] + 1] != "#"){ #Desno
  validDirections[2] = 1
}
if(m1[pos[1] - 1, pos[2]] != "#"){ #Gor
  validDirections[3] = 1
}
if(m1[pos[1] + 1, pos[2]] != "#"){ #Dol
  validDirections[4] = 1
}

#Change that element to a random number between the lower and upper bounds
change <- FALSE
while(!(change)){
  new <- runif(1, 0, 4.2)
  if(validDirections[ceiling(new)] == 1){
    mutate[j] <- new
    change <- TRUE
  }

}

return(mutate)
}

myCrossover <- function(object, parent1, parent2){
  outcome1 <- parent1 <- as.vector(object@population[parent1,])
  outcome2 <- parent2 <- as.vector(object@population[parent2,])
  n <- length(parent1)
  #Sample a random vector element that should be changed
  j <- sample(1:n, size = 2)
  spMeja <- min(j)
  zgMeja <- max(j)

  for(i in spMeja:zgMeja){
    temp <- outcome1[i]
    outcome1[i] <- outcome2[i]
    outcome2[i] <- temp
  }

  children <- matrix(1:2, nrow = 2)
  children[1:1] <- outcome1
  children[1:2] <- outcome2
  fitness <- rep(NA,2)

  return(list(children = children, fitness = fitness))
}

GA <- ga(type = "real-valued", fitness = fitnes,lower = lBound, upper = uBound, popSize = 1500, pmutation = 0.7, maxiter = 1000, mutation = myMutation, crossover = myCrossover)
plot(GA)

resitev <- floor(GA@solution[1,])
resitevPrint(resitev)
resitev
