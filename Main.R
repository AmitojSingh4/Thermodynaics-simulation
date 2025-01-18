# size of lattice (n x n)
n <- 10

# number of quanta / pixel (integer)
Q <- 1

# make initial lattice
A <- array(Q, dim = c(n, n))

J <- 1


GetEnergy <- function(i,j) {
  energy <- -J*(A[i,j]*(GetValue(i+1,j)+GetValue(i-1,j)+GetValue(i,j+1)+GetValue(i,j-1)))
  return(energy)
}

GetValue <- function(a,b) {
  if ((0<a&a<n)&(0<b&b<n)) {
    return(A[a,b])
  }
  else {
    return(0)
  }
}

print(GetEnergy(4,4))