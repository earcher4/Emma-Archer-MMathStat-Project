#takes the population of each district
dist_pop = function(eta){
  d = c()
  for(i in 1:13){
    d[i] = sum(D$Population[eta==i]) #the sum of each precinct population within the distrcit
  }  
  return(d)
}

######################################

C1 = function(eta){
  d = c()
  for(i in 1:13){
    d[i] = dist_pop(eta)[i]- (1/13)*sum(dist_pop(eta)) #difference between the district population and the ideal population
  } 
  return(C1 = sum(d^2)/50000000) #the sum of squared difference for each district, scaled for comparision with other score parameters 
}

######################################
