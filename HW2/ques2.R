
#If dataset is not suitable for linear regression then we can use non-parametric regression
model <- lm(Sepal.Length ~ Sepal.Width, data = iris)
plot(iris$Sepal.Width, iris$Sepal.Length, main = "Scatterplot with Regression Line", col = 'blue', pch = 16, xlab = "Sepal.Width", ylab = "Sepal.Length")
abline(model, col = 'red', lwd = 2)



Y<- iris$Sepal.Length
x <- iris$Sepal.Width

kernel <- density(x)
plot(kernel)
b <- kernel$bw
n <- length(Y)

x2 <- seq(min(x),max(x),by=0.001)


#local mean
fn_mean <- function(t, Y, x, x0, b)
{
  n <- length(x)
  sum <- sum((Y-t[1]-t[2]*(x-x0)-t[3]*((x-x0)^2)/2)^2*dnorm((x-x0)/b)*dnorm((x-x0)/b))
  return(sum)
}

val <- NULL

for (x0 in x2) {
  init <- c(mean(x), mean(x), mean(x))
  r <- optim(par = init, fn = fn_mean, Y = Y, x = x, b = b, x0 = x0, method = 'L-BFGS-B')
  val <- rbind(val, c(x0, r$par, r$value))  
}

plot(x=x,y=Y,pch=16,col="blue",xlab="Sepal width", ylab="Sepal length",main="Non-parametric regression using local polynomial mean")
points(x=val[,1], y=(val[,2]), type='l', col="red")
legend("topright", legend = c("Data points", "Expected Y"), col = c("blue", "red"), pch = c(16, NA), lty = 1, cex = 0.8)

y_range <- 0.1*(range(val[,2:4]))

plot(x=val[,1], y=0.1*(val[,2]), type='l', col="red",ylim=y_range, ylab="Y*0.1", xlab="X",main="Non-parametric regression using local polynomial mean")
points(x=val[,1], y=0.1*(val[,3]), type='l', col='green')
points(x=val[,1], y=0.1*(val[,4]), type='l', col='yellow')
legend("topright",legend=c("Estimated Y", "Estimated dY/dX","Estimated d2Y/dX2"), col=c('red','green','yellow'), lty = 1, cex = 0.8)


#local meadian
fn_median <- function(t, Y, x, x0, b)
{
  n <- length(x)
  sum <- sum(abs(Y-t[1]-t[2]*(x-x0)-t[3]*((x-x0)^2)/2)*dnorm((x-x0)/b)*dnorm((x-x0)/b))
  return(sum)
}

val <- NULL

for (x0 in x2) {
  init <- c(mean(x), mean(x), mean(x))
  r <- optim(par = init, fn = fn_median, Y = Y, x = x, b = b, x0 = x0, method = 'L-BFGS-B')
  val <- rbind(val, c(x0, r$par, r$value))  
}

plot(x=x,y=Y,pch=16,col="blue",xlab="Sepal width", ylab="Sepal length",main="Non-parametric regression using local polynomial median")
points(x=val[,1], y=(val[,2]), type='l', col="red")
legend("topright", legend = c("Data points", "Expected Y"), col = c("blue", "red"), pch = c(16, NA), lty = 1, cex = 0.8)

y_range <- 0.1*(range(val[,2:4]))

plot(x=val[,1], y=0.1*(val[,2]), type='l', col="red",ylim=y_range, ylab="Y*0.1", xlab="X",main="Non-parametric regression using local polynomial median")
points(x=val[,1], y=0.1*(val[,3]), type='l', col='green')
points(x=val[,1], y=0.1*(val[,4]), type='l', col='yellow')
legend("topright",legend=c("Estimated Y", "Estimated dY/dX","Estimated d2Y/dX2"), col=c('red','green','yellow'), lty = 1, cex = 0.8)


##estimated values of x LP mean
est_mean <- NULL
for (pt in x) {
  init <- c(mean(x), mean(x), mean(x))
  r <- optim(par = init, fn = fn_mean, Y = Y, x = x, b = b, x0 = pt, method = 'L-BFGS-B')
  est_mean <- rbind(est_mean, r$par[2])  
}

mse_mean <-  mean((Y-est_mean)^2)

##estimated values of x LP median
est_median <- NULL
for(pt in x)
{
  init <- c(mean(x), mean(x), mean(x))
  r <- optim(par = init, fn = fn_median, Y = Y, x = x, b = b, x0 = pt, method = 'L-BFGS-B')
  est_median <- rbind(est_median, r$par[2])  
}

mse_median <- mean((Y-est_median)^2)

##comparison
if(mse_mean < mse_median)
{
  cat("LP mean is better")
}else 
{
  cat("LP median is better")
}



