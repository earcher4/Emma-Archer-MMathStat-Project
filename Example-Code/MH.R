MH = function(n,eta){
  count = 1
  beta = c()
  beta = geombeta(n)
  p = c()
  p[1] = exp(-beta[1]*J(eta)) #initial mass 
  dem = c()
  dem[1]=dem_total(eta) #number of democrat districts in plan
  U = runif(n-1)
  plan = matrix(NA,ncol=100,nrow=n)
  plan[1,] = eta 
  for(i in 2:n){
    eta = eta
    #plots(eta)
    eta_new = proposed(eta)
    alpha = (conflicted(eta)/conflicted(eta_new))*exp(-beta[i]*(J(eta_new)-J(eta))) 
    if(log(U[i-1])<log(alpha)){
      eta = eta_new  #accept new plan with probably min(1,alpha)
    } else {
      eta = eta  #otherwise reject new plan 
    }
    #plots(eta) #a visualisation of each itteration
    plan[i,] = eta #update plan
    p[i] = exp(-beta[i]*J(eta)) #update probability
    dem[i] = dem_total(eta) #update number of democratic seats won 
    count = count + 1 #a count to see which iteration the algorithm is completing
    print(count)
  }
  list(democrat = dem,plan = plan,p = p/sum(p))
}



start.time <- Sys.time()
f = MH(30000,eta)
end.time <- Sys.time()              
time.taken <- end.time - start.time
time.taken



eta = c(3,rep(1,6),11,12,13,3, rep(2,6),11,12,13,rep(3,6),4, 11,12,13,rep(4,7),11,12,13,rep(5,7),11,12,13,rep(6,7),11,12,13,rep(7,7),11,12,13,rep(8,7),11,12,13, rep(9,7),11,12,13, rep(10,7),11,12,13)
