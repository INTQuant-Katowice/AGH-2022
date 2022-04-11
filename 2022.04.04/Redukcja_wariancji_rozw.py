import numpy as np
import numpy.random as rnd
import matplotlib.pylab as plt
import matplotlib.pyplot as pyplot
import time

def geometric_brownian_paths(n_paths, n_steps_per_year, r, sigma, T, S0, method = "Standard", theta=0):

    dt = 1/n_steps_per_year
    n_steps = n_steps_per_year * T

    if method=="Antithetic Sampling":
        Np = rnd.randn(int(n_paths/2), n_steps)
        N_ = -Np
        N = np.vstack((Np,N_))
    else:
        N = rnd.randn(n_paths, n_steps)

    if method == "Importance Sampling":
        M = N + theta*dt
        S = np.full((n_paths, n_steps + 1), S0)
        R = np.full((n_paths, n_steps + 1), 1.0)

        for i in range(n_paths):
            for j in range(n_steps):
                S[i, j+1] = S[i,j] * np.exp((r-sigma**2/2)*dt + sigma*np.sqrt(dt)*M[i,j])
                R[i, j+1] = R[i,j] * np.exp(-theta*N[i,j]*np.sqrt(dt) - theta**2/2*dt)

        paths = np.hstack((S, R))
    else:
        paths = np.full((n_paths, n_steps + 1), S0)
        for i in range(n_paths):
            for j in range(n_steps):
                paths[i, j+1] = paths[i,j] * np.exp((r-sigma**2/2)*dt + sigma*np.sqrt(dt)*N[i,j])
    return paths

def monte_carlo_price(n_paths, n_steps_per_year, r, sigma, T, S0, strike, product="call", method="Standard", theta=0):

    paths = geometric_brownian_paths(n_paths, n_steps_per_year, r, sigma, T, S0, method, theta)

    if method=="Importance Sampling":
        X = paths[:, int(np.shape(paths)[1]/2.0)-1]
    else:
        X = paths[:, -1]

    if product=="call":
        Y = np.maximum(X - strike, 0)
    elif product=="put":
        Y = np.maximum(strike - X, 0)
    elif product=="forward":
        Y = X
    if method == "Control Variates":
        b = np.cov(X,Y)[0][1]/np.std(X)**2
        price = np.exp(-r*T)*np.mean(Y-b*(X-np.exp(r*T)*S0))
    elif method == "Importance Sampling":
        price = np.exp(-r * T) * np.mean(Y*paths[:,-1])
    else:
        price = np.exp(-r * T)*np.mean(Y)

    return price

n_paths = [1000,5000,10000,50000,100000]


T = 1
n_seeds = 100
methods = ["Standard", "Antithetic Sampling", "Control Variates", "Importance Sampling"]

theta = 2.5

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
    plt.show

run_convergence_analysis(n_paths, 1, 0.02, 0.15, 1, 100.0, 150.0, "call", n_seeds)