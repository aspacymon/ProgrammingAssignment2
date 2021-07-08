CacheMatrix <- function(a = matrix()){
  inv <- NULL
  set <- function(b){
    a <<- b
    inv <<- NULL
  }
  get <- function() {a}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(a, ...){
  inv <- a$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mtx <- a$get()
  inv <- solve(mtx, ...)
  a$setInverse(inv)
  inv
}