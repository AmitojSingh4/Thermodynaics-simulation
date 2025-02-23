n <- 3
lattice <- array(sample(c(-1,1),size = n*n, replace=TRUE), dim = c(n,n))

magnetisation <- function(lattice) {
  M <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      M <- M + lattice[i,j]
    }
  }
  return(M/(n^2))
}

Epsilon <- function(i,j,array) { #Energy of a single element
  if ((0<i&i<=n)&(0<j&j<=n)) {
    energy <- (array[i,j]*(GetValue(i+1,j,array)+GetValue(i-1,j,array)+GetValue(i,j+1,array)+GetValue(i,j-1,array)))
  }
  else {
    energy <- 0
  }
  return(energy)
}

GetValue <- function(a,b,array) { #Sanitation for edge cases
  if ((0<a&a<=n)&(0<b&b<=n)) {
    return(array[a,b])
  }
  else {
    return(0)
  }
}

total_energy <- function(S, J=1) { #Total energy of the entire lattice
  E <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      E <- E + Epsilon(i,j,S)
    }
  }
  return(-J*E)
}

print(total_energy(lattice))