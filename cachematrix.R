## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	
  cachevalue <- NULL ## This creates an empty cache vector, named 'cachevalue', to store the inverse matrix later
  
  get <- x ## This vector contains the original matrix used in the MakeCacheMatrix function
  
  setcache <- function(newcache) cachevalue <<- newcache ## This function stores the calculated inverse in the cache vector
  
  getcache <- function() cachevalue ## This function retrieves whatever is stored in the cache vector at the time when it is called
  
  list(get = get, setcache = setcache, getcache = getcache) ## This creates a list to organize the matrix, get, and functions, setcache and getcache, so that they can be called later by the cacheSolve function
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
	
  isthereacache <- x$getcache() ## This retrieves whatever is stored in the cache vector
  
  if(!is.null(isthereacache)) {
    message("Retrieving the inverse matrix from cache!")
    return(isthereacache)
  } ## If there is something stored in the cache vector, it is returned
  
  matrix <- x$get  ## If there is nothing stored in the cache vector, the function pulls the original matrix ...
  newcache <- solve(matrix, ...) ## ... calculates its inverse ...
  x$setcache(newcache) ## ... stores the calculated inverse in the cache vector for future reference ...
  newcache ## ... then returns the calculated inverse!
  }
