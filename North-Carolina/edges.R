library(rgeos)
library(rgdal)
polys <- readOGR("Voting_Precincts (1).geojson")
adj <- gTouches(polys, byid = TRUE)   #logical matrix of adjacent edges 

adj[which(adj== FALSE)] = 0
adj[which(adj== TRUE)] = 1 

edges = adj #binary matrix of adjacent edges 
