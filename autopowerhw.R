

rollreduce <- function(M, V, A, Crolling = .015, G = 9.8, Pair = 1.2 , Cdrag = .3 ) { return (
  .5*Crolling * M * G * V + .5*A * Pair *Cdrag * V^3
)} 

autopwr <- function(M, V, A, Crolling = .015, G = 9.8, Pair = 1.2 , Cdrag = .3 ) { return (
  Crolling * M * G * V + .5*A * Pair *Cdrag * V^3
)} 
