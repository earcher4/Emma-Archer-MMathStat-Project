## function which return the perimeter of each district
per = function(eta){
  counts = c() 
  l = c()
  p = c()
  for(i in 1:13){ 
    b =(which(eta == i)) #precincts in district i
    for(j in 1:length(b)){
      l[j] = length(intersect(which(edges[b[j],]  == 1),(which(eta == i)))) #number edges that are dshared by precinct within the same district
    }
    p[i] = sum(D$Perimeter[b]) - sum(l) #sum of perimeter of each edge in district - perimeter of edges shared within the district
    l = c()
  }
  return(p)
}

#######################################################

