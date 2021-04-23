createCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  gMatrix <- function() {
    x
  }
  cacheSet <- function(inverse) {
    cache <<- inverse
  }
  gCache <- function() {
    cache
  }
  list(gMatrix = gMatrix, cacheSet = cacheSet, gCache = gCache)
}


solveCache <- function(x, ...) {
  cache <- x$ggCache()
  if (is.null(cache)) {
    y <- x$gMatrix()
    cache <- solve(y, ...)
    x$cacheSet(cache)
    return(cache)
  }
  cache
}
