

geometric_brownian_paths <- function(n_paths, n_steps_per_year, r, sigma, T, S0, method="Standard", theta=0){
  dt = 1/n_steps_per_year
  n_steps = n_steps_per_year*T
  
  if (method=="Antithetic Sampling"){
    ## 1. miejsce na stworzenie macierzy zmiennych losowych, gdzie po³owa jest wygenerowana z generatora,
    ##    a druga po³owa jest utworzona jako liczby przeciwne.
    N = matrix(rnorm(n_paths*n_steps,mean=0,sd=1),n_paths, n_steps)
  }
  else {
    N = matrix(rnorm(n_paths*n_steps,mean=0,sd=1),n_paths, n_steps)
  }
  
  if (method == "Importance Sampling") {
    ## 2. miejsce na uworzenie zmiennej paths zawierajêcej œcie¿ki potrzebne do policzenia cen
    ##    metod¹ "importance sampling"
    paths = matrix(nrow=n_paths, ncol=n_steps+1,S0)
  }
  else {
    paths = matrix(nrow=n_paths, ncol=n_steps+1,S0)
    for (i in 1:n_paths){
      for (j in 1:n_steps) {
        paths[i,j+1] = paths[i,j]*exp((r-sigma^2)/2*dt+sigma*sqrt(dt)*N[i,j])
      }
    }
  }
  return(paths)
  
}

monte_carlo_price <- function(n_paths, n_steps_per_year, r, sigma, T, S0, strike, product="call", method="Standard", theta=0){
  paths = geometric_brownian_paths(n_paths, n_steps_per_year, r, sigma, T, S0, method, theta)
  if (method=="Importance Sampling")
  {
    ## 3. miejsce na definicje œcie¿ek cen akcji potrzbenych do wyceny metod¹ "importance sampling"
    X = paths[,ncol(paths)]
  }
  else 
  {
    X = paths[,ncol(paths)]
  }
  if (product=="call")
  {
    ## 4. miejsce na definicjê realizacji wyp³at opcji call w oparciu o zmienn¹ X
    Y = X
  }  
  else if (product=="put"){
    ## 5. miejsce na definicjê realizacji wyp³at opcji call w oparciu o zmienn¹ X
    Y = X
  }
  else if (product=="forward"){
    Y=X
  }
  if (method=="Control Variates"){
    ## 6. miejsce na policzenie cen opcji (price) metod¹ control variates
    price = 0.0
   }
   else if (method=="Importance Sampling"){
    ## 7. miejsce na policzenie cen opcji (price) metod¹ importance sampling
    price = 0.0
   }
  {
    price = exp(-r*T)*mean(Y)
  }
  return(price)
}


##n_paths = c(100,500,1000,5000,10000,50000,100000)
n_paths = c(1000,5000,10000)
T = 1
n_seeds = 100
methods = c("Standard")
#methods = c("Standard", "Antithetic Sampling", "Control Variates", "Importance Sampling")

## 8. miejsce na wybór theta do metody importance sampling
theta = 0.0


run_convergence_analysis <- function(n_paths,n_steps_per_year,r,sigma,T,S0,K,option_type,n_seeds){
  sd_standard = c()
  sd_as = c()
  sd_is = c()
  sd_cv = c()
  
  for (n in n_paths)
  {
    avg_standard = c(rep(1:n_seeds))
    #avg_as = c(rep(1:n_seeds))
    #avg_is = c(rep(1:n_seeds))
    #avg_cv = c(rep(1:n_seeds))
    for (i in c(rep(1:n_seeds))){
      avg_standard[i] <- monte_carlo_price(n, n_steps_per_year, r, sigma, T, S0, K, option_type, "Standard")
      #avg_as[i] <- monte_carlo_price(n, n_steps_per_year, r, sigma, T, S0, K, option_type, "Antithetic Sampling")
      #avg_cv[i] <- monte_carlo_price(n, n_steps_per_year, r, sigma, T, S0, K, option_type, "Control Variates")
      #avg_is[i] <- monte_carlo_price(n, n_steps_per_year, r, sigma, T, S0, K, option_type, "Importance Sampling", theta=0.5)
    }
    sd_standard <- c(sd_standard, sd(avg_standard))
    #sd_as <- c(sd_as, sd(avg_as))
    #sd_cv <- c(sd_cv, sd(avg_cv))
    #sd_cv <- c(sd_is, sd(avg_is))
  }

  #sd_max = max(max(sd_standard), max(sd_as), max(sd_is), max(sd_cv))
  #sd_min = min(min(sd_standard), min(sd_as), min(sd_is), min(sd_cv))
  plot(n_paths, sd_standard, type="l", col="red", log="x") #, ylim=range(sd_min, sd_max))
  #lines(n_paths, sd_as, type="l",col="yellow", log="x")
  #lines(n_paths, sd_cv, type="l",col="blue", log="x")
  #lines(n_paths, sd_is, type="l",col="green", log="x")
}

run_convergence_analysis(n_paths,1,0.02,0.15,1,100,125,"call",100)