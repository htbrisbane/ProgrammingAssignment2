## This function is to create a special "matrix" object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## This function computes the inverse of a special "matrix" created by makeCacheMatrix above. 
## Return from cache if the inverse has already been calculated (and the matrix has not changed), 
## otherwise compute inverse and store in its cache for  future use.
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("Already inversed, getting cached data.")
    return(m)
  }
  message("New matrix, inversing the matrix.")
  matrix <- x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
