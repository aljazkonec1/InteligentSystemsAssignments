library(tidyverse)
library(GA)
source("mazes.r")

m <- maze5_T

{
  
  {  m <- str_split (m, "", Inf, simplify = TRUE)
  start <- which(m == "S", arr.ind = TRUE)
  finish <- which(m == "E", arr.ind = TRUE)
  posE <- finish
  }
  
  fitnes <- function(v_ukazi) { 
    score <- 0
    pos <- start
    
    v_ukazi <- floor(v_ukazi)
    m1 <- m
    for (i in 1: length(v_ukazi)) {
      
      if(pos[1] == 1 || pos[2] == 1){
        print(pos)
        print(m1[pos])
      }
      
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
  
  myMutation <- function(object, parent){
    m1 <- m
    outcome <- parent <- as.vector(object@population[parent,])
    n <- length(parent)
    j <- sample(1:n, size = 1)
    pos <- start
    
    parent <- floor(parent)
    
    for (i in 1:j) {
      
      if(pos[1] == 1 || pos[2] == 1){
        print(pos)
        print(m1[pos])
      }
      
      m1[pos] = "#"
      
      if(parent[i] == 0 & m1[pos[1], pos[2] - 1] != "#" ){ # korak v levo
        pos[2] <- pos[2] - 1
      }
      else if(parent[i] == 1 & m1[pos[1], pos[2] + 1] != "#"){  # korak v desno
        pos[2] <- pos[2] + 1
      }
      else if(parent[i] == 2 & m1[pos[1] - 1, pos[2]] != "#"){  # korak gor
        pos[1] <- pos[1] - 1
      }
      else if(parent[i] == 3 & m1[pos[1] + 1, pos[2]] != "#"){ # korak dol
        pos[1] <- pos[1] + 1
      }
      
      if(m[pos] == "E"){ # ce pride do cilja, ga ne mutiraj
        return(outcome)
      }
      
    }
    
    validDirections <- c(0,0,0,0)
    obstajaValid <- FALSE
    
    if(m1[pos[1], pos[2] - 1] != "#"){ #Levo
      validDirections[1] = 1
      obstajaValid <- TRUE
    }
    if(m1[pos[1], pos[2] + 1] != "#"){ #Desno
      validDirections[2] = 1
      obstajaValid <- TRUE
    }
    if(m1[pos[1] - 1, pos[2]] != "#"){ #Gor
      validDirections[3] = 1
      obstajaValid <- TRUE
    }
    if(m1[pos[1] + 1, pos[2]] != "#"){ #Dol
      validDirections[4] = 1
      obstajaValid <- TRUE
    }
    
    change <- FALSE
    while(!(change) && obstajaValid){
      new <- runif(1, 0, 4)
      if(validDirections[ceiling(new)] == 1){
        outcome[j] <- new
        change <- TRUE
      }
      
    }
    
    return(outcome)
  }
  
  myCrossover <- function(object, parent1, parent2){
    outcome1 <- parent1 <- as.vector(object@population[parent1,])
    outcome2 <- parent2 <- as.vector(object@population[parent2,])
    n <- length(parent1)
    
    j <- sample(1:n, size = 2)
    spMeja <- min(j)
    zgMeja <- max(j)
    zacetekDrugi <- sample(1:spMeja, size = 1)
    
    for(i in spMeja:zgMeja){
      temp <- outcome1[i]
      outcome1[i] <- outcome2[zacetekDrugi + i - spMeja]
      outcome2[zacetekDrugi + i - spMeja] <- temp
    }
    
    children <- matrix(1:2, nrow = 2)
    children[1:1] <- outcome1
    children[1:2] <- outcome2
    fitness <- rep(NA,2)
    
    return(list(children = children, fitness = fitness))
  }
  
  printPoteze <- function(x) {
    r <- c()
    m1 <- m
    pos <- start
    for ( i in 1:nSteps) {
      if(x[i] == 0 & m1[pos[1], pos[2] - 1] != "#" ){ # korak v levo
        r <- c(r, "L")
        pos[2] <- pos[2] - 1
      }
      else if(x[i] == 1 & m1[pos[1], pos[2] + 1] != "#"){  # korak v desno
        r <- c(r, "R")
        pos[2] <- pos[2] + 1
      }
      else if(x[i] == 2 & m1[pos[1] - 1, pos[2]] != "#"){  # korak gor
        r <- c(r, "U")
        pos[1] <- pos[1] - 1
      }
      else if(x[i] == 3 & m1[pos[1] + 1, pos[2]] != "#"){ # korak dol
        r <- c(r, "D")
        pos[1] <- pos[1] + 1
      } 
      
      if ( m[pos] == "E") {
        return(r)
      }
    }
    return(r)
  }
  
  manhatan <- function(a, b) {
    sum(abs(a - b))
  }
  
  { t0 <- Sys.time()
    t <- which(m== "T", arr.ind = TRUE)
    len <- c()
    if(!length(t) == 0){
      for (i in 1:length(t[,1])){
        len <- c(len,  manhatan(t[i,], start))
      }
    }
    
    t <- cbind(t, len)
    t <- t[order(t[, 3]), ]
    
    t <- rbind(c(start, 0), t)
    t <- rbind(t, c(finish, 0))
    t <- t[, 1:2]
    
    m[t] <- "."
    m[posE] <- "#"
    
    v <- c()
  }
  
  for (i in 2:length(t[,1])){
    m[t[i-1,1], t[i-1,2]] <- "S"
    m[t[i,1],t[i,2]]<- "E"
    start <- which(m== "S", arr.ind = TRUE)
    finish <- which(m== "E", arr.ind = TRUE)
    start
    msg <- c("Finding path from:", start, "to:", finish)
    print(msg)
    nSteps <- sum(m == ".") +1
    lBound <- rep(0, nSteps)
    uBound <- rep(4, nSteps)
    GA <- ga(type = "real-valued", fitness = fitnes, lower = lBound, upper = uBound, maxiter= 100, popSize = 2000, pmutation = 1, pcrossover = 0.5)#, mutation = myMutation), crossover = myCrossover)
    if(GA@iter>1){
      plot(GA)
    }
    v <- c(v,printPoteze(floor(GA@solution[1, ])))
    m[t] <- "."
    m[posE] <- "#"
  }
  
  {print(v)
  
  time <- round(as.double( difftime(Sys.time(), t0, u = 'secs')))
  
  text <- c("This program took ", time, "seconds")
  print(text)
  }
  
}

