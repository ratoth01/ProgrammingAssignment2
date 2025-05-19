#This program creates the makeCachematrix function that checks if the inverse
#of a matrix has be previously calculated and cached.  It uses mcache as a
#global list of lists where each element contains a matrix and its previously
#calculated inverse.  Cachesolve is an embedded function that receives a
#single matrix (my_matrix).  It uses a for loop to cycle through each list
#element of mcache to check if my_matrix is stored in any of the lists.  If it
#finds mcache, it returns the inverse (second element).  If it does not, then
#it calculates the inverse of my_matrix and assigns it to msolve.  It then
#appends mcache with a new list (my_matrix,msolve) and returns msolve.  
#makeCachematrix then returns a list containing cachesolve.
#

#initialize mcache with a placeholder (otherwise would need to add is.na
#functionality to cachesolve)

ph <- matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),4,4)#placeholder matrix
mcache <- list(list(ph,solve(ph)))


makeCachematrix <- function() {
  
  #Begin cache function
  
  cachesolve <- function(my_matrix){
    #loop through the mcache list to check if my_matrix is cached
    for (element in mcache)
      if (identical(element[[1]], my_matrix)) {
        print("Cache Hit")
        return(element[[2]])  # return cached inverse
      }
    #if not in cache, execute the following:
    msolve <-solve(my_matrix)  # calculate and return the inverse
    mcache[[length(mcache)+1]] <<- list(my_matrix,msolve)  #update the cache
    return(msolve) #return the calculate inverse
    
  } # closed bracket for cachesolve
  
  return(list(cachesolve = cachesolve))
  
} # closed bracket for makeCachematrix
