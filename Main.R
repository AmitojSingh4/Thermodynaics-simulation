
CreateArray <- function(n){
  A <- array(Q, dim = c(n, n))
  for (i in 1:n) {
    for (j in 1:n) {
      Q <- sample(c(-1,1),size = 1)
      A[i,j] <- Q
    }
  }
  return(A)
}

Epsilon <- function(i,j) { #Energy of a single atom
  energy <- -J*(A[i,j]*(GetValue(i+1,j)+GetValue(i-1,j)+GetValue(i,j+1)+GetValue(i,j-1)))
  return(energy)
}

GetValue <- function(a,b) { #Sanitation for edge cases
  if ((0<a&a<n)&(0<b&b<n)) {
    return(A[a,b])
  }
  else {
    return(0)
  }
}

E <- function() { #Total energy of the entire lattice
  E <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      E <- E + Epsilon(i,j)
    }
  }
  return(E)
}

M <- function() { #Total magnetization of the lattice
  M <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      M <- M + A[i,j]
    }
  }
  return(M)
}

Main <- function() {
  n <<- 10 #Size of lattice (n x n)
  J <<- 1
  A <<- CreateArray(n)
  print(E())
  print(M())
}

Main()