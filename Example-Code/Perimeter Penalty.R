#function assigns a score to difference in peremiter lengths 

C3 = function(eta){
  d = matrix(NA,nrow=13,ncol=13)
  p = c()
  for(i in 1:13){
    for(j in 1:13){
      d[i,j] = per(eta)[i] - per(eta)[j]  #creates a matrix of perimeter differences between each districts
    }
  }
  return(sum(d^2)/8) #sum of the squared difference 
}



#######################################################
