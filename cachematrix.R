## Matrix inversion is usually a costly computation 
##and there may be some benefit to caching 
##the inverse of a matrix rather than compute it repeatedly.
## The next two functions are used to cache the inverse of a matrix

## The first function "makeCacheMatrix"
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to "cacheSolve()" that is the secound function 

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
}

    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse
         )
}


# The secound function "cacheSolve" returns the inverse of the original matrix input to makeCacheMatrix(),
##so this function is the output of makeCacheMatrix() .
#It first checks the inverse has already been computed. 
#If so, it gets the result and skips the computation. 
#If not, it computes the inverse, sets the value in the cache via setinverse function.


cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  
}

### test 
x = rbind(c(1, -2), c(-2, 1))
x
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)
cacheSolve(m)

