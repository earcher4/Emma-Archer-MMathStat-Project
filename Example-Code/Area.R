#area per district
area_dist = function(eta){
  area_dist = c()
  for(i in 1:13){
    area_dist[i] = sum(D$Area[eta==i])  #sum of the area of each precinct within the district
  }
  return(area_dist)
}

###############################
