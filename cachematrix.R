## Creates special object 'cacheMatrix' 
## and deal with it for getting inverse of the matrix

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(msource = matrix()) { 
        if(is.matrix(msource)){
                
                functionToApply <- NULL
                set <- function(y) 
                {
                        msource <<- y
                        functionToApply <<- NULL
                }
                get <- function() msource
                setSolve <- function(solve) functionToApply <<- solve
                getSolve <- function() functionToApply
                list(set = set, get = get,
                     setSolve = setSolve,
                     getSolve = getSolve)
        }
        else { NULL }
}

## Get function or it's cached version that returns inverse of the matrix;

cacheSolve <- function(cachedMatrix, ...) {
        
        if(!is.atomic(cachedMatrix) && !is.null(cachedMatrix)){
                
                FUN <- cachedMatrix$getSolve()
                if(!is.null(FUN)) {
                        return(FUN)
                }
                data <- cachedMatrix$get()
                FUN <- solve(data, ...)
                cachedMatrix$setSolve(FUN)
                FUN
        }
        else { print("check your input")}
}