C2 = function(eta){
  d = c()
  for (i in 1:13){
    d[i] = (per(eta)[i]^2/(area_dist(eta)[i])) #ratio of area of a circle whos circumference is the perimeter to the area 
  }
  return(C2 = (sum(d)/length(eta) - 1)*100) #sum of the ratios
}

#########################################################