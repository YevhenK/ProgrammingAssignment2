## Totally, this function was created to inverse the matrices

## This function going to create a special "matrix", which:
## set values of the matrix, then get this values
## set inverse of a matrix, then get this inverse
## Sorry for my english, it is bad :(

makeCacheMatrix <- function(x = matrix())
                  {m <- NULL
                   set <- function(y)
                         {x <<- y
                          m <<- NULL
                         }
                   get <- function() x
                   setinvers <- function(solve) m[,] <<- solve
                   getinvers <- function() m[,]
                   list(set=set,get=get,setinvers=setinvers,getinvers=getinvers)
                  }


## This function creating the inverse of "matrix" from previous function.
## First it checks to see if the inverse has already been in cache.
## If so, it gets it and skips the computation.
## Otherwise, it creating the inverse of the matrix
## and set it in the cache by setinverse function.

cacheSolve <- function(x, ...)
             {m <- x$getinvers()
              if(!is.null(m)) {message("getting cached data")
                               return(m)
                              }
              data <- x$get()
              m <- solve(data, ...)
              x$setinvers(m)
              m
             }