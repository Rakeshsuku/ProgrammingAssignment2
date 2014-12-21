## This function creates a special matrix that provides an option to cache its inverse.
## So the system need nod perform calculations everytime the inverse of the matrix is required. 
## Inverse is calculated only once(first time its required) and stored in the memory.


## This function makeCacheMatrix creates a list of four functions used to do the following:
## 1. Set the values of the matrix x
## 2. Retrive the values of the matrix x
## 3. Set the values of the inverse matrix inv
## 4. Get the values of the inverse matix inv
## The matrix and its inverse are stored local to the function makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {

	inv <-NULL;

	set <- function (y)  {
		x <<- y;
		inv <<- NULL;
		## '<<' is used because inv and x are defined in the parent function.
	}

	get <- function () {
		x;
	}

	setinv <- function (inverse) {
		inv <<- inverse;
                ## '<<' is used because inv is defined in the parent function.
	}

	getinv <- function ()  {
		inv;
	}

	list (set = set, get = get, setinv = setinv, getinv = getinv);

}


## The function cachesolves first checks if the inverse of the matrix is already calculated and cached.
## If the inverse is already cached, the fuction returns the inverse from the memory.
## It the inverse is not cached, the function calculates the inverse, caches the inverse matrix and returns the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inv <- x$getinv ();
	## Retrieves cahced data. Is NULL unless inverse is already cached. 
        
	if (!is.null(inv))  {
		message ("Getting cached data");
		return (inv);
	}


	matrix <- x$get ();
	inverse <- solve (matrix);
        ## Calculates the inverse of the matrix.
	x$setinv (inverse);
        ## Caches the inverse
	inverse;
        
}
