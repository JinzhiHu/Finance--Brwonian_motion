## Task 1.0: Calculate the realized stock price at time t
##  Yt: the entire path of the stock price

Data <- read.csv("Changyu.csv")

## Logged-change on adjusted close
mu <- mean(diff(log(na.omit(Data$Adj.Close))))
sigma <- sd(diff(log(na.omit(Data$Adj.Close))))

## We adapt with the in-practice estimation for p, u, and d
n = 100
dt = 1 / 12 ## Monthly data
u = exp(sigma * sqrt(dt))
d = 1 / u
pu = 1 / 2 + 1 / 2 * mu / sigma * sqrt(dt)

## We generate a Brownian motion (Using cumsum)
set.seed(123)
Z <- rnorm(n, 0, dt)
W <- cumsum(c(0, sqrt(dt) * Z))

## Brownian motion with drift mu and volatility sigma
t <- seq(from = 0, to = n * dt, by = dt)
Xt <- mu * t + sigma * W

## Geometric Brownian motion
Y0 <- as.numeric(Data$Open[nrow(Data)]) ## We get the opening of the first 
                                        ##  trading day
Yt <- Y0 * exp(Xt)

## Task 2.0: Plot the path
plot(t, Yt, type="l")
