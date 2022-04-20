#returns the number of edges which are between different districts

conflicted = function(eta){
  count = 0
  for(q in 1:100){
    for (p in 1:100){
      if((edges[p,q] > 0)&(eta[p] != eta[q])){ #if there exists and edge and the edge ius beytween two different districts
        count = count +1
      }
    }
  }
  return(count)
}
