## Assumption: All the input matrices are invertible
## makeCacheMatrix returns a list that contains function to :
## 1. set value of matrix
## 2. get value of matrix
## 3. set inverse of matrix
## 4. get inverse of matrix

## This function takes a matrix as input, sets its value by caching it through 'set' function
## 'get' function returns this matrix  
## 'setinverse' function gives a value to inverse matrix 'i' by caching it further.
## 'getinverse' function gives the inverse matrix 'i' as output 
## A list containing all these functions is returned in the end. 

makeCacheMatrix <- function(x = matrix()) {
                    i<-NULL
                    set<-function(y){
                      x<<-y
                      i<<-NULL
                    }
                    get<-function() x
                    setinverse<-function(inv) i<<-inv
                    getinverse<-function() i
                    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The below function returns the inverse of a matrix. The output of 'makeCacheMatrix'
## function, which is a list, is input to this function. First, it checks whether an inverse 
## matrix is already present, through an 'if' condition. If
## inverse matrix is not present as cached data, it takes the input matrix through 'get' function and 
## calculates inverse matrix through 'solve' function and returns it,
## also assigning this same value to 'setinverse' function of the list.
## If inverse matrix is already present, it fetches and returns it from cached data without recalculating it.
## Result is the inverse of the input matrix

cacheSolve <- function(x, ...) {
              i<-x$getinverse()
              if(!is.null(i)){
                  message("getting cached data...")
                  return(i)
              }
              mat<-x$get()
              i<-solve(mat)
              x$setinverse(i)
              i

}
