## "cachematrix" is written in R language and provides functions for fast 
##  computing inverse of square matrices assumption the determinant is non-zero. 
##  The package depends on caching the inverse matrices and provides the cached 
##  results when required for the next call, e.g. in a loop.
##  Also, this package provides unit testing code and section for git comments 
##  to push the source code to GitHub.


## Function 1: Create a list of four elements for maintaining the cached matrix 
## and its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function (y) {
                x <<- y
                i <<- NULL
        }
        get <- function () x
        setinverse <- function (inverse) i <<- inverse
        
        
        getinverse <- function () i
        list (set = set, get = get,
              setinverse = setinverse, getinverse = getinverse)
        
}


## Function 2: return the cached inverse matrix if already computed; otherwise, 
## compute and cache the inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse ()
        if (!is.null (i)) {
                message ("Inverse is cached")
                return (i)
        }
        message ("Inverse is computed and NOT cached")
        data <- x$get()
        i <- solve (data)
        x$setinverse (i)
        i
}



#UNIT TESTING
#Testing without caching
z <- matrix(c(4,2,7,6), 2, 2)
solve (z)

#testing with caching function
zC <- makeCacheMatrix (z)
cacheSolve(zC)  #compute inverse matrix. Printed message is ("Inverse is computed and NOT cached").
cacheSolve(zC)  #return cached inverse matrix. Printed message is ("Inverse is cached").

  


#SECTION for GIT
#downsteam from a fork git
#git init
#git clone https://github.com/rdpeng/ProgrammingAssignment2.git
#git remote -v

#create a new repo in githib
#git remote add origin https://github.com/aounihajar/ProgrammingAssignment2.git
#git remote -v
#git pull origin master --allow-unrelated-histories
#git add .
#git commit -m "R Programming, week 3 comment"
#git push --set-upstream origin master

#uploading a file for a second time
#git add .
#git commit -m "R Programming, week 3 comment"
#git push --set-upstream origin master