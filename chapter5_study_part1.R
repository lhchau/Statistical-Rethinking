library(rethinking) # load data
data("WaffleDivorce")
d <- WaffleDivorce

# standardize variables
d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)

sd(d$MedianAgeMarriage)

m5.1 <- quap(
  alist(
    D     ~ dnorm(mu, sigma),
    mu    <- a + bA * A,
    a     ~ dnorm(0, 0.2),
    bA    ~ dnorm(0, 0.5),
    sigma ~ dexp(1)),
  data = d
)

set.seed(23)
prior <- extract.prior(m5.1)
mu <- link(m5.1, post = prior, data = list(A = c(-2, 2)))
plot(NULL, xlim = c(-2, 2), ylim = c(-2, 2))
for(i in seq_len(50)) lines(c(-2, 2), mu[i, ], col = col.alpha("black", 0.5))

A_seq <- seq(from = -3, to = 3.2, length.out = 30)
mu <- link(m5.1, data = list(A = A_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(D ~ A, data = d, col = rangi2)
lines(A_seq, mu.mean, lwd = 2)
shade(mu.PI, A_seq)

m5.2 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM * M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

library(dagitty)
dag5.1 <- dagitty( "dag{ A -> D; A -> M; M -> D }" )
coordinates(dag5.1) <- list( x=c(A=1,D=1,M=2) , y=c(A=0,D=1,M=1) )
drawdag( dag5.1 )

m5.3 <- quap(
  alist(
    D       ~ dnorm(mu, sigma),
    mu      <- a + bM*M + bA*A,
    a       ~ dnorm(0, 0.2),
    bM      ~ dnorm(0, 0.5),
    bA      ~ dnorm(0, 0.5),
    sigma   ~ dexp(1)
  ), data = d
)

precis(m5.3)

plot( coeftab(m5.1,m5.2,m5.3), par=c("bA","bM") )

m5.4 <- quap(
  alist(
    M     ~ dnorm(mu, sigma),
    mu    <- a + bAM * A,
    a     ~ dnorm(0, 0.2),
    bAM   ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)

b5.4 <- 
  brm(data = d,
      family = gaussian,
      M ~ 1 + A,
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(uniform(0, 10), class = sigma)),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 5,
      file = "fits/b05.04")

precis(m5.4)

# call link without specifying new data
# so it uses original data
mu <- link( m5.3 )
# summarize samples across cases
mu_mean <- apply( mu , 2 , mean )
mu_PI <- apply( mu , 2 , PI )
# simulate observations
# again no new data, so uses original data
D_sim <- sim( m5.3 , n=1e4 )
D_PI <- apply( D_sim , 2 , PI )

plot( mu_mean ~ d$D , col=rangi2 , ylim=range(mu_PI) ,
      xlab="Observed divorce" , ylab="Predicted divorce" )
abline( a=0 , b=1 , lty=2 )
for ( i in 1:nrow(d) ) lines( rep(d$D[i],2) , mu_PI[,i] , col=rangi2 )
identify( x=d$D , y=mu_mean , labels=d$Loc )




















