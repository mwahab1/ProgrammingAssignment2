## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix is a function that takes a square matrix(does not check) 
#as its arguement  and  when invoked, creates a list of 4 functions for 
#geting/setting  cached matrix and for getting/setting the inverse


makeCacheMatrix <- function(x = matrix()) {
      
      inv <- NULL #initialize cached inverse matrix to null
      set <- function(y) { 
            x <<- y      #update matrix
            inv <<- NULL #clear cache
      }
      
      
      get <- function() x # return matrix (invoked by cacheSolve)
      setinv <- function(Inverse) inv <<- Inverse  #update the cached inverse 
      getinv <- function() inv        #return the cached inverse
      
      # create a list with these functions
      list (set= set, get = get,
            setinv = setinv, getinv=getinv )
      
}



# Obtains the inverse matrix from cache. If the cached inverse is null
# calculates the inverse using the information stored in x.
# Once an inverse is calculated, updates the cache and returns the inverse matrix
# If a cached inverse matrix is found...that matrix is returned

cacheSolve <- function(x, ...) {
      inv <- x$getinv() #obtain rfom cache
      
      if(!is.null(inv)) { # check if the matrix read from cache is null 
            message("getting cached data")
            return(inv) 
            
      }
      
      # There isnt a cached inverse matrix. so calculate.
      data <- x$get()            #read the matrix
      inv <-  solve(data, ...)   #calculate the inverse
      
      x$setinv(inv) #update the cache
      
      
      inv  ## Return a matrix that is the inverse of 'x'
}
