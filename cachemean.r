## This code has two objectives:
##  1.) Create a matrix which can cache data,
##        then produce the inverse the cached data. 
##  2.) Compute the inverse value of the cached data, if the inverse value
##        does not already exist.
##
## Objective #1
makeCachedMatrix <- function(m = matrix()){   ##Defines function for matrix creation
	cd <- NULL                                ##Initializes variable for cached data to zero
	set <- function(s){                       ##Sets the value of the matrix
		m <<- s                               ##
		cd <<- NULL                           ##
	}                                         ##
	get <- function() m                       ##Get the value of the matrix
	setinv <- function(solve) cd <<- solve    ##Invert matrix value
	getinv <- function() cd                   ##Cache inverted value
	list(set = set, get = get,                ##Creates list vector of returned values
		setinv = setinv,                      ##
		getinv = getinv)                      ##
}                                             ##End of function definition

## Objective #2
cacheSolve <- function(m = matrix(), ...) {   ##Start retrieval routine
	cd <- m$getinv()                          ##Retrieve cached data call
	if(!is.null(cd)) {                        ##Does cached data exist?
		message("Getting cached data")        ##Notice of retrieval
		return(cd)                            ##Retrieve cached data
	}                                         ##End retrieval routine
	data <- m$get()                           ##If cached data does not exist, get data
	cd <- solve(data, ...)                    ##Invert data
	m$setinv(cd)                              ##Perform calculation
	cd                                        ##Return value to console
}                                             ##End of function definition