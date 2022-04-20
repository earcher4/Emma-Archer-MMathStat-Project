MH = function(n,eta){
  count = 1
  beta = c()
  beta = geombeta(n)
  p = c()
  p[1] = exp(-beta[1]*J(eta)) #initial mass 
  dem = c()
  dem[1]=dem_total(eta) #number of democrat districts in plan
  U = runif(n-1)
  plan = matrix(NA,ncol=length(eta),nrow=n)
  plan[1,] = eta 
  for(i in 2:n){
    eta = eta
    eta_new = proposed(eta)
    alpha = (conflicted(eta)/conflicted(eta_new))*exp(-beta[i]*(J(eta_new)-J(eta))) 
    if(log(U[i-1])<log(alpha)){
      eta = eta_new  #accept new plan with probably min(1,alpha)
    } else {
      eta = eta  #otherwise reject new plan 
    }
    plan[i,] = eta #update plan
    p[i] = exp(-beta[i]*J(eta)) #update probability
    dem[i] = dem_total(eta) #update number of democratic seats won 
    count = count + 1 #a count to see which iteration the algorithm is completing
    print(count)
  }
  list(democrat = dem,plan = plan,p = p/sum(p))
}



start.time <- Sys.time()
ee = MH(1000,e$plan[1000,])
end.time <- Sys.time()              
time.taken <- end.time - start.time
time.taken



