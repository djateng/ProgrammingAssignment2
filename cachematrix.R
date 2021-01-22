makeCacheMatrix <-function(m = matrix()){
  inv <- NULL
  set <- function(y){
    m <<-y
    inv <<-NULL
  }
  get <-function(){m}
  setInverse <-function(inverse){inv <<- inverse}
  getInverse <- function(){inv}
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
cacheSolve <-function(m, ...){
  inv <- m$getInverse()
  if(!is.null(inv)){
    message("Getting Cashed Data")
    return(inv)
  }
  matr <- m$get()
  inv <- solve(matr, ...)
  m$setInverse(inv)
}