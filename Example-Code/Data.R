library("readxl")
library("dplyr")
D = read_excel("Pseudo_data.xlsx")

###########################################

#empty matrix for shared edges
edges = matrix(0,nrow = dim(D), ncol = dim(D)+1)
colnames(edges) = c(sprintf("Precinct %i", 1:100),"border")
rownames(edges) = sprintf("Precinct %i", 1:100)

##binary matrix where 1 indicates a shared edge
for(i in 1:100){
  if(i %in% c(2:9)){
    edges[i,c(i-1,i+1,i+10,101)] = 1
  } else if(i %in% c(92:99)){
    edges[i,c(i-1,i+1,101)] = 1
  } else if (i %in% 1){
    edges[i,c(i+1,i+10,101)]=1
  } else if (i %in% 10){
    edges[i,c(i+10,i-1,101)]=1
  } else if (i %in% c(20,30,40,50,60,70,80,90)){
    edges[i,c(i+10,i-10,i-1,101)]=1
  } else if (i %in% c(11,21,31,41,51,61,71,81)){
    edges[i,c(i+1,i+10,i-10,101)]=1
  } else if (i %in% 91){
    edges[i,c(i+1,i-10, 101)]=1
  } else if (i %in% 100){
    edges[i,c(i-1,i-10,101)]=1
  }else{
    edges[i,c(i+1,i-1,i-10,i+10)] = 1
  }
}

###########################################

# original plan
eta = D$congressional_district

###########################################
