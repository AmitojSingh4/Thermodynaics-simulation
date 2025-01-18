
CreateArray <- function(n){ #Creates array of size nxn 
  A <- array(Q, dim = c(n, n))
  for (i in 1:n) {
    for (j in 1:n) {
      Q <- sample(c(-1,1),size = 1)
      A[i,j] <- Q
    }
  }
  return(A)
}

Epsilon <- function(i,j,array) { #Energy of a single element
  if ((0<i&i<n)&(0<j&j<n)) {
  energy <- -J*(array[i,j]*(GetValue(i+1,j,array)+GetValue(i-1,j,array)+GetValue(i,j+1,array)+GetValue(i,j-1,array)))
  }
  else {
    energy <- 0
  }
  return(energy)
}

GetValue <- function(a,b,array) { #Sanitation for edge cases
  if ((0<a&a<n)&(0<b&b<n)) {
    return(array[a,b])
  }
  else {
    return(0)
  }
}

E <- function(array) { #Total energy of the entire lattice
  E <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      E <- E + Epsilon(i,j,array)
    }
  }
  return(E)
}

M <- function(array) { #Total magnetization of the lattice
  M <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      M <- M + array[i,j]
    }
  }
  return(M)
}

Flip <- function() { #Flips the spin of a random element
  i <- sample(c(1:n),size = 1)
  j <- sample(c(1:n),size = 1)
  ATemp <<- A
  if (A[i,j]==1) {
    ATemp[i,j] <<- -1
  }
  else {
    ATemp[i,j] <<- 1
  }
  return(array(c(i,j), dim = c(1,2)))
}

DeltaE <- function() {
  EBefore <- E(A)
  FlipData <- Flip()
  FEI <- FlipData[1,1] #Flipped Element I
  FEJ <- FlipData[1,2] #Flipped Element J
  LocalEBefore <- Epsilon(FEI,FEJ,A) + Epsilon(FEI+1,FEJ,A) + Epsilon(FEI-1,FEJ,A) + Epsilon(FEI,FEJ+1,A) + Epsilon(FEI,FEJ-1,A)
  LocalEAfter <- Epsilon(FEI,FEJ,ATemp) + Epsilon(FEI+1,FEJ,ATemp) + Epsilon(FEI-1,FEJ,ATemp) + Epsilon(FEI,FEJ+1,ATemp) + Epsilon(FEI,FEJ-1,ATemp)
  EAfter <- EBefore - LocalEBefore + LocalEAfter
  dE <- EAfter - EBefore
  return(dE)
}

Main <- function() {
  n <<- 10 #Size of lattice (n x n)
  J <<- 1 #Coupling constant
  A <<- CreateArray(n)
  image(A)
  print(DeltaE())
}

Main()