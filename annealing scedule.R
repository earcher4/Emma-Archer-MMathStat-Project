geombeta = function(n){
  beta = c()
  beta[1] = 0.001
  for(i in 1:(n-1)){
    if(beta[i] < 0.1){
      beta[i+1] = 1/(0.999995^(i))-1
    } else {
      beta[i+1] = beta[i]
    }
  }
  beta
}

geombeta = function(n){
  beta = c()
  beta[1] = 1/(0.999995^(1999))-1
  for(i in 1:(n-1)){
    if(beta[i] < 0.1){
      beta[i+1] = 1/(0.999995^(i+1999))-1
    } else {
      beta[i+1] = beta[i]
    }
  }
  beta
}