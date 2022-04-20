#the weighted sum of our penalty functions 

J = function(eta){
  J = C1(eta) + 0.5*C2(eta)  + C3(eta)  
  return(J)
} 
