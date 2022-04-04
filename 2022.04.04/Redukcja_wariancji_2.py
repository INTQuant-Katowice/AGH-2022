import numpy as np
import numpy.random as rnd
import matplotlib.pylab as plt
import matplotlib.pyplot as pyplot
import time

def geometric_brownian_paths(n_paths, n_steps_per_year, r, sigma, T, S0, method = "Standard", theta=0):

    dt = 1/n_steps_per_year
    n_steps = n_steps_per_year * T

    if method=="Antithetic Sampling":
        ## 1. miejsce na stworzenie macierzy zmiennych losowych, gdzie połowa jest wygenerowana z generatora,
        ##    a druga połowa jest utworzona jako liczby przeciwne.
        N = rnd.randn(n_paths, n_steps)
    else:
        N = rnd.randn(n_paths, n_steps)

    if method == "Importance Sampling":
        ## 2. miejsce na uworzenie zmiennej paths zawierajęcej ścieżki potrzebne do policzenia cen
        ##    metodą "importance sampling"
        paths = np.full((n_paths, n_steps + 1), S0)
    else:
        paths = np.full((n_paths, n_steps + 1), S0)
        for i in range(n_paths):
            for j in range(n_steps):
                paths[i, j+1] = paths[i,j] * np.exp((r-sigma**2/2)*dt + sigma*np.sqrt(dt)*N[i,j])
    return paths

def monte_carlo_price(n_paths, n_steps_per_year, r, sigma, T, S0, strike, product="call", method="Standard", theta=0):

    paths = geometric_brownian_paths(n_paths, n_steps_per_year, r, sigma, T, S0, method, theta)

    if method=="Importance Sampling":
        ## 3. miejsce na definicje ścieżek cen akcji potrzbenych do wyceny metodą "importance sampling"
        X = paths[:, -1]
    else:
        X = paths[:, -1]

    if product=="call":
        ## 4. miejsce na definicję realizacji wypłat opcji call w oparciu o zmienną X
        Y = X
    elif product=="put":
        ## 5. miejsce na definicję realizacji wypłat opcji call w oparciu o zmienną X
        Y = X
    elif product=="forward":
        Y = X
    if method == "Control Variates":
        ## 6. miejsce na policzenie cen opcji (price) metodą control variates
        price = 0.0
    elif method == "Importance Sampling":
        ## 7. miejsce na policzenie cen opcji (price) metodą importance sampling
        price = 0.0
    else:
        price = np.exp(-r * T)*np.mean(Y)

    return price

#n_paths = [100,500,1000,5000,10000,50000,100000]
n_paths = [1000,10000]
T = 1
n_seeds = 100
methods = ["Standard", "Antithetic Sampling"]
#methods = ["Standard", "Antithetic Sampling", "Control Variates", "Importance Sampling"]

## 8. miejsce na wybór theta do metody importance sampling
theta = 0.0

def run_convergence_analysis(n_paths,n_steps_per_year,r,sigma,T,S0,K,option_type,n_seeds):

    plt.figure()
    results = np.full((n_seeds,len(methods)), 0.0)
    for k, method in enumerate(methods):
        print(method)
        sd = []
        for j, n in enumerate(n_paths):
            prices = []
            start = time.time()
            for i in range(n_seeds):
                price = monte_carlo_price(n, n_steps_per_year, r, sigma, T, S0, K, option_type, method, theta if method=="Importance Sampling" else 0)
                prices.append(price)
            end = time.time()
            if n==n_paths[-1]:
                print("czas: ", end-start)
            results[:,k] = prices
            sd.append(np.std(prices))
        plt.plot(n_paths, sd, label=method)
    plt.legend()
    plt.show()
    plt.figure()
    pyplot.boxplot(results, labels=methods)
    plt.show()

run_convergence_analysis(n_paths, 1, 0.02, 0.15, 1, 100.0, 150.0, "call", 100)