## This code has two objectives:
##  1.) Create a matrix which can cache data,
##        then produce the inverse the cached data. 
##  2.) Compute the inverse value of the cached data, if the inverse value
##        does not already exist.
##
##
##
## Objective #1
makeCachedMatrix <- function(x = matrix()){   
	cd <- NULL                                ##Cached data
	set <- function(s){                       
		x <<- s                               
		cd <<- NULL                           
	}                                         
	get <- function() x                       
	setinv <- function(solve) cd <<- solve    
	getinv <- function() cd                   
	list(set = set, get = get,                
		setinv = setinv,                      
		getinv = getinv)                      
}                                             

##
##
## Objective #2
cacheSolve <- function(x = matrix(), ...) {   
	cd <- x$getinv()                          
	if(!is.null(cd)) {                        
		message("Getting cached data")        
		return(cd)                            
	}                                         
	data <- x$get()                           
	cd <- solve(data, ...)                    
	x$setinv(cd)                              
	cd                                        
}                                             