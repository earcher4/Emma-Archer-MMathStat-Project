
edge.coord = function(samp = r.sample){
  c((samp-1)%%100+1,floor((samp-1)/100)+1)
}

#############################################

#returns which precinct is connected to the imput and in the same district as the imput
dis.conn = function(precinct,eta_new){
  dis = eta_new[precinct] # District of the precinct
  prec.edges = which(edges[precinct,] == 1) # Edges connected to precinct
  dis.prec.edges = eta_new[prec.edges] # Districts of precinct edges
  out <- prec.edges[which(dis.prec.edges == dis)] # Precincts connected and in the same district
  out
}

#############################################

#binary indicator function where one indicates the district is not dissconected
is.dis.conn <- function(district,eta_new){
  precincts <- which(eta_new == district)
  no.precincts <- length(precincts)
  chain.ind <- 0
  chain <- precincts[1]
  curr.chain <- chain
  chain.resolved <- c()
  while(chain.ind == 0){
    chain <- sort(unique(c(chain,dis.conn(curr.chain,eta_new))))
    chain.resolved <- sort(unique(c(chain.resolved,curr.chain)))
    if(length(chain) == length(chain.resolved)){chain.ind <- 1}else{curr.chain <- setdiff(chain,chain.resolved)[1]}
  }
  if(length(chain) == no.precincts){connected <- 1}else{connected <- 0}
  return(connected)
}

#############################################
