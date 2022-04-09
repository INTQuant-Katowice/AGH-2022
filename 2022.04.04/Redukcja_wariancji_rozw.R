

geometric_brownian_paths <- function(n_paths, n_steps_per_year, r, sigma, T, S0, method="Standard", theta=0){
  dt = 1/n_steps_per_year
  n_steps = n_steps_per_year*T
  
  if (method=="Antithetic Sampling"){
    Np = matrix(rnorm(n_paths*n_steps,mean=0,sd=1),n_paths/2, n_steps)
    N_ = -Np
    N = rbind(Np,N_)
  }
  else {
    N = matrix(rnorm(n_paths*n_steps,mean=0,sd=1),n_paths, n_steps)
  }
  
  if (method == "Importance Sampling") {
    M = N + theta*dt
    Spaths = matrix(nrow=n_paths,ncol=n_steps+1,S0)
    RNderiv = matrix(nrow=n_paths, ncol=n_steps+1,1)
    for (i in 1:n_paths){
      for (j in 1:n_steps) {
        Spaths[i,j+1] = Spaths[i,j]*exp((r-sigma^2/2)*dt + sigma*sqrt(dt)*M[i,j])
        RNderiv[i,j+1] = RNderiv[i,j]*exp(-theta*N[i,j]*sqrt(dt)-theta^2/2*dt)
      }
    }
  
  paths = cbind(Spaths, RNderiv)
  }
  else {
    paths = matrix(nrow=n_paths, ncol=n_steps+1,S0)
    for (i in 1:n_paths){
      for (j in 1:n_steps) {
        paths[i,j+1] = paths[i,j]*exp((r-sigma^2/2)*dt+sigma*sqrt(dt)*N[i,j])
      }
    }
  }
  return(paths)
}

monte_carlo_price <- function(n_paths, n_steps_per_year, r, sigma, T, S0, strike, product="call", method="Standard", theta=0){
  paths = geometric_brownian_paths(n_paths, n_steps_per_year, r, sigma, T, S0, method, theta)
  
  if (method=="Importance Sampling"){
    X = paths[,ncol(paths)/2]
  }
  else {
    X = paths[,ncol(paths)]
  }
  if (product=="call"){
    Y = pmax(X-strike, 0)
  }  
  else if (product=="put"){
    Y = pmax(strike-X, 0)
  }
  else if (product=="forward"){
    Y=X
  }
   if (method=="Control Variates"){
     b = cov(X,Y)/sd(X)^2
     price = exp(-r*T)*mean(Y-b*(X-exp(r*T)*S0))
   }
   else if (method=="Importance Sampling"){
     price = exp(-r*T)*mean(Y*paths[,ncol(paths)])
   }
  else
  {
    price = exp(-r*T)*mean(Y)
  }
  return(price)
}


n_paths = c(1000,5000,10000,50000,100000)

T = 1
n_seeds = 100
theta = 2.5

methods = c("Standard", "Antithetic Sampling", "Control Variates", "Importance Sampling")


run_convergence_analysis <- function(n_paths,n_steps_per_year,r,sigma,T,S0,K,option_type,n_seeds){

  results = matrix(nrow=n_seeds,ncol=length(methods), 0.0)
  
  sds = matrix(nrow=length(n_paths), ncol=length(methods), 0.0)
  
  for (k in 1:length(methods))
  {
    print(methods[k])
    sd = c()
    
    
    for (j in 1:length(n_paths)){
      
      prices = c()
      start_time <- Sys.time()
      for (i in 1:n_seeds){
        
        price = monte_carlo_price(n_paths[j], n_steps_per_year, r, sigma, T, S0, K, option_type, methods[k], theta)
        prices<- c(prices, price)
      }
    end_time <- Sys.time()  
    if (n_paths[j] == n_paths[length(n_paths)]){
      print(end_time - start_time)
    }
    results[, k] = prices
    sd <- c(sd,sd(prices))
    }

    sds[,k] = sd
    
  }
    dataf = as.data.frame(results)
    colnames(dataf) = methods
    dataf_std = as.data.frame(sds)
    colnames(dataf_std) = methods
    boxplot(results, xlab=methods)
    matplot(n_paths, dataf_std, type="l")
    legend("top", methods,col=seq_len(4), lty=1:4)
  
}

run_convergence_analysis(n_paths,1,0.02,0.15,1,100,150,"call",n_seeds)