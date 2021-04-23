createCacheMatrix <- function(x = matrix()) {
  
  cachecache <- NULL                                                  ## starts as Null or 0
  
  gMatrix <- function() {                                             ## get smatrix
    
    x
    
  }
  
  cacheSet <- function(inverse) {                                   ##s set cahce
    
    cachecache <<- inverse
    
  }
  
  gCache <- function() {                                                  ## get chache
    
    cachecache
    
  }
  
  list(gMatrix = gMatrix, cacheSet = cacheSet, gCache = gCache)           ## list of everthing is equal to everything
}


solveCache <- function(x, ...) {                                        ## this is the code for solving the cache matrix
  
  cachecache <- x$ggCache()                                             
  
  if (is.null(cachecache)) {
    
    y <- x$gMatrix()
    
    cachecache <- solve(y, ...)
    
    x$cacheSet(cachecache)
    
    return(cachecache)
    
  }
  
  cachecache
}
