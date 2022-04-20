proposed = function(eta){
  edge.coordinates = sapply(which(edges == 1),edge.coord) #matrix of adjacent districts
  flip = edge.coordinates[,sample(dim(edge.coordinates)[2],size = 1)] #selecting a pair of precincts to flip 
  eta_new = eta
  if(runif(1)>0.5){  # with probability 0.5 swap the first precinct to the same district as the second precinct 
    w = flip[1]
    o = flip[2]
  }else{            # with probability 0.5 swap the second precinct to the same district as the first precinct 
    w = flip[2]
    o = flip[1]
  }
  eta_new[w] = eta_new[o] #swap the selected precinct 
  l = c()
  for(j in 1:13){l[j] = length(which(eta_new == j))} #vector of amount of precincts per district
  while (eta[o]==eta[w] || is.dis.conn(eta_new[w],eta_new) == 0 || is.dis.conn(eta[w],eta_new) == 0 || any(l<130) || any(l>280)){
     # if any of the following are true re-select precincts to flip : precinct pair is originally in same districts, flipping the first precinct creates a discotinous district, flipping the second precinct creates a discotinous district, a district contains less than 5 precincts, a district contains more than 10 precincts  
    flip = edge.coordinates[,sample(dim(edge.coordinates)[2],size = 1)] #selecting a pair of precincts to flip 
    eta_new = eta
    if(runif(1)>0.5){ # with probability 0.5 swap the first precinct to the same district as the second precinct
      w = flip[1]
      o = flip[2]
    }else{           # with probability 0.5 swap the second precinct to the same district as the first precinct 
      w = flip[2]
      o = flip[1]
    } 
    eta_new[w] = eta_new[o]  #swap the selected precinct
    for(j in 1:13){l[j] = length(which(eta_new == j))} #vector of amount of precincts per district
  }
  return(eta_new)
}


