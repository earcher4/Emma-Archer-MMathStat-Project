
dem_total = function(eta){
  winner = c()
  for(i in 1:13){
    if (sum(D$`Dem vote`[which(eta == i)]) > sum(D$`Rep vote`[which(eta == i)])){ #if the sum of districts where Dem vote % is higher than Rep vote %
      winner[i] = "DEM" #lables winner as DEM
    } else{
      winner[i] = "REP" #lables winner as REP
    }
  }
  return(length(winner[winner == "DEM"])) #number of districts with DEM winner
}

