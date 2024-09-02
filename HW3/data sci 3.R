set.seed(45869)

# Parameters
T <- 1       # Total time
N <- 1000    # Number of time steps
dt <- T / N  # Time step size
mu <- 1   # Drift coefficient (mean increment per unit time)
sigma <- 0.2 # Diffusion coefficient (standard deviation of increments)

# Generate Brownian motion
dW_brownian <- rnorm(N, mean = 0, sd = sqrt(dt))
W_brownian <- cumsum(dW_brownian)

# Generate drifted Brownian motion
dW_drifted <- rnorm(N, mean = mu * dt, sd = sigma * sqrt(dt))
W_drifted <- cumsum(dW_drifted)



# Combine both processes with specified weights
W_combined <- 0.7 * W_brownian + 0.3 * W_drifted

# Plot
plot(seq(0, T, length.out = N), W_combined, type = "l", xlab = "Time", ylab = "Position", main = "Brownian Motion",
     ylim = c(-2.5,3),col="deeppink")
n <- 50
V <- matrix(0,N,n)
V[,1] <- W_combined

for(i in 2:n)
{
  dW_brownian <- rnorm(N, mean = 0, sd = sqrt(dt))
  W_brownian <- cumsum(dW_brownian)
  
  # Generate drifted Brownian motion
  dW_drifted <- rnorm(N, mean = mu * dt, sd = sigma * sqrt(dt))
  W_drifted <- cumsum(dW_drifted)
  
  # Combine both processes with specified weights
  V[,i] <- 0.8 * W_brownian + 0.2 * W_drifted
  
  # Plot
  lines(seq(0, T, length.out = N), V[,i], type = "l", xlab = "Time", ylab = "Position", main = "Brownian Motion", col="deeppink")
  
}

upper_band <- apply(V, 1, function(path) quantile(path, 0.975))
lower_band <- apply(V, 1, function(path) quantile(path, 0.025))

lines(seq(0, T, length.out = N), upper_band, col = "navyblue", lwd=2)
lines(seq(0, T, length.out = N), lower_band, col = "navyblue", lwd = 2)


iter <- 0
for(i in 1:n)
{
  brow <- V[,i]
  out <- 0
  for(j in 1:N)
  {
    if(brow[j] > upper_band[j] || brow[j] < lower_band[j])
      out <- out+1
  }
  if (out > N/2)
  {
    iter <- iter + 1
  }
}
out95 <- iter/n*100

# Plot
plot(seq(0, T, length.out = N), W_combined, type = "l", xlab = "Time", ylab = "Position", main = "Combined Brownian Motion",
     ylim = c(-2.5,3),col="skyblue")
n <- 50
V <- matrix(0,N,n)
V[,1] <- W_combined

for(i in 2:n)
{
  dW_brownian <- rnorm(N, mean = 0, sd = sqrt(dt))
  W_brownian <- cumsum(dW_brownian)
  
  # Generate drifted Brownian motion
  dW_drifted <- rnorm(N, mean = mu * dt, sd = sigma * sqrt(dt))
  W_drifted <- cumsum(dW_drifted)
  
  # Combine both processes with specified weights
  V[,i] <- 0.8 * W_brownian + 0.2 * W_drifted
  
  # Plot
  lines(seq(0, T, length.out = N), V[,i], type = "l", xlab = "Time", ylab = "Position", main = "Combined Brownian Motion", col="skyblue")
  
}

upper_band <- apply(V, 1, function(path) quantile(path, 0.95))
lower_band <- apply(V, 1, function(path) quantile(path, 0.05))

lines(seq(0, T, length.out = N), upper_band, col = "darkgreen", lwd=2)
lines(seq(0, T, length.out = N), lower_band, col = "darkgreen", lwd = 2)


iter <- 0
for(i in 1:n)
{
  brow <- V[,i]
  out <- 0
  for(j in 1:N)
  {
    if(brow[j] > upper_band[j] || brow[j] < lower_band[j])
      out <- out+1
  }
  if (out > N/2)
  {
    iter <- iter + 1
  }
}
out90 <- iter/n*100

T <- 1       # Total time
N <- 1000    # Number of time steps
dt <- T / N  # Time step size
mu <- 1   # Drift coefficient (mean increment per unit time)
sigma <- 0.2 # Diffusion coefficient (standard deviation of increments)

# Generate Brownian motion
dW_brownian <- rnorm(N, mean = 0, sd = sqrt(dt))
W_brownian <- cumsum(dW_brownian)

# Generate drifted Brownian motion
dW_drifted <- rnorm(N, mean = mu * dt, sd = sigma * sqrt(dt))
W_drifted <- cumsum(dW_drifted)



# Combine both processes with specified weights
W_combined <- 0.7 * W_brownian + 0.3 * W_drifted

V <- matrix(0,N,n)
V[,1] <- W_combined

for(i in 2:n)
{
  dW_brownian <- rnorm(N, mean = 0, sd = sqrt(dt))
  W_brownian <- cumsum(dW_brownian)
  
  # Generate drifted Brownian motion
  dW_drifted <- rnorm(N, mean = mu * dt, sd = sigma * sqrt(dt))
  W_drifted <- cumsum(dW_drifted)
  
  # Combine both processes with specified weights
  V[,i] <- 0.8 * W_brownian + 0.2 * W_drifted
}

xbar <- function(alpha)
{
  return(mean(apply(V[(floor(n*alpha)+1):(n-floor(n*alpha)),],1,mean)))
}
xbar(.28)
alpha <- seq(0,0.5,0.001)
xbaralpha <- sapply(alpha,xbar)
plot(alpha,xbaralpha,xlab=expression(alpha),ylab = "Trimmed Mean",col="blue")
abline(v=.28,col="magenta",lwd=2)

# q2

library(Metrics)
# Parameters
T <- 1 # Total time
N <- 1000 # Number of time steps
dt <- T / N # Time step size
n <- 100

# Generate random increments
dW <- rnorm(N, mean = 0, sd = sqrt(dt))


# Cumulative sum to obtain Brownian motion
W <- cumsum(dW)

# Plot
plot(seq(0, T, length.out = N), W, type = "l", xlab = "Time",
     ylab = "Position", main = "Brownian Motion", ylim = c(-2.5,3))

V <- matrix(0,N,n)
V[,1] <- W

for(i in 2:n)
{
  dW <- rnorm(N, mean = 0, sd = sqrt(dt))
  V[,i] <- cumsum(dW)
  
  # Plot
  lines(seq(0, T, length.out = N), V[,i], type = "l",lwd = 2, xlab = "Time", ylab = "Position", main = "Brownian Motion", col = "magenta")
  
}

#generating Y
Y = colMeans(V^2) + rnorm(ncol(V))
Y


#dividing into testing and training data sets
train_index <- sample(1:n, 0.7*n)  # 70% of data for training


Y_train = Y[train_index]
V_train = V[, train_index]

Y_test = Y[-train_index]
V_test = V[, -train_index]

m_hat = function(f, h, Y, V)
{
  norms = apply(f-V, 2, FUN = norm, type = '2') 
  kd = dnorm(norms/h)
  return(sum(kd*Y)/sum(kd))
}


#----------------CHANGE THIS BANDWIDTH ACC. TO YOUR DATA-------------
h = 10 #bandwidth 

#fitting the model on training data
m_hat_values_train = numeric(length = length(Y_train))

for(i in 1:length(Y_train)){
  m_hat_values_train[i] = m_hat(V_train[,i], h, Y_train, V_train)
}

#RMSE of the training dataset
(err1 = rmse(m_hat_values_train, Y_train))

#fitting the model on testing data
m_hat_values_test = numeric(length = length(Y_test))
for(i in 1:length(Y_test)){
  m_hat_values_test[i] = m_hat(V_test[,i], h, Y_train, V_train)
}

#RMSE of the testing dataset
(err2 = rmse(m_hat_values_test, Y_test))


