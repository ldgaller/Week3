makeVector <- function(x = numeric()) {
    #begin by setting the mean to NULL as a placeholder for a future value
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #returns the vector, x
    get <- function() x
    #sets the mean, m, to mean
    setmean <- function(mean) m <<- mean
    #returns the mean, m
    getmean <- function() m
    #returns the 'special vector' containing all of the functions just defined
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}
makeCacheMatrix <- function(x = matrix()) { 
        #begin by setting the mean to NULL as a placeholder for a future value
        inv <- NULL 
        set <- function(y) { 
                 x <<- y 
                 inv <<- NULL 
             } 
         get <- function() x 
         setinverse <- function(inverse) inv <<- inverse 
         getinverse <- function() inv 
         list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
     } 
cacheSolve <- function(x, ...) { 
         inv <- x$getinverse() 
         if(!is.null(inv)) { 
                 message("getting cached data.") 
                 return(inv) 
             } 
         data <- x$get() 
         inv <- solve(data) 
         x$setinverse(inv) 
         inv 
     } 
