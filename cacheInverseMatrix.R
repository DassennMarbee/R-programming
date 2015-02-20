## Example: 
## mtrx <- matrix(rnorm(100,mean = 12, sd = 4), c(10,10))
## obj <- makeCacheMatrix()
## obj$set(mtrx)
## inv <- cacheSolve(obj)
## inv2 <- cacheSolve(obj) ## Using cached data


makeCacheMatrix <- function(x = matrix()) {
  ## Return a list of objects, which allow the inverse of the matrix x to be cached.
  inv <- NULL
  set <- function(y){x<<-y;inv<<-NULL}
  get <- function()x
  setinv<-function(inve)inv<<-inve
  getinv<-function()inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){message("Using cached data"); return(inv)}
  mtrx<-x$get()
  inv<-solve(mtrx,...)
  x$setinv(inv)
  inv
}