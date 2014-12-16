##Calculates and caches the inverse of a matrix

##Create the set/get framework for cache & return
##inv_x = inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
      inv_x <- NULL      
      set <- function(y){
            x <<- y
            inv_x <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv_x <<- inverse
      getinv <- function() inv_x
      list (set = set, get = get, setinv = setinv, getinv = getinv)
}

##Get inverse from cache if possible, else solve, cache,
##and return inverse

cacheSolve <- function(x, ...) {
      inv_x <- x$getinv()
      if(!is.null(inv_x)) {
            message("I pity the fool who didn't cache this inverse!")
            return(inv_x)
      }
      data <- x$get()
      inv_x <- solve(data, ...)
      x$setinv(inv_x)
      inv_x
}
