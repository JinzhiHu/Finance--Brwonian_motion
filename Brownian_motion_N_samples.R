## Task 3.0: Expand tasks 1.0 and 2.0 in `Brownian_motion.R` into 
##  a sampling of N Brownian motions
N <- 100
set.seed(123)

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
t <- seq(from = 0, to = n * dt, by = dt)
BM <- vector("list", N)
for (i in 1:N) {
  ## We generate a Brownian motion (Using cumsum)
  
  Z <- rnorm(n, 0, dt)
  W <- cumsum(c(0, sqrt(dt) * Z))
  
  ## Brownian motion with drift mu and volatility sigma
  
  Xt <- mu * t + sigma * W
  
  ## Geometric Brownian motion
  Y0 <- as.numeric(Data$Open[nrow(Data)]) ## We get the opening of the first 
  ##  trading day
  Yt <- Y0 * exp(Xt)
  if (i == 1) {
    plot(t, Yt, type="l", ylim = c(8.9, 10.2))
  } else {
    lines(t, Yt, type="l")
  }
}

