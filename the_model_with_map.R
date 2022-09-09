library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

flist <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu ~ dnorm( 178 , 20 ) ,
  sigma ~ dunif( 0 , 50 )
)

m4.1 <- map( flist , data=d2 )
precis( m4.1 )

m4.2 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu ~ dnorm( 178 , 0.1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d2 )
precis( m4.2 )

# Khi thay doi sd cua mu thi sau khi map, gia tri cua sigma m4.2 thay doi kha nhieu so voi m4.1
library(MASS)
post <- mvrnorm( n=1e4 , mu=coef(m4.1) , Sigma=vcov(m4.1) )

m4.1_logsigma <- map(
  alist(
    height ~ dnorm( mu , exp(log_sigma) ) ,
    mu ~ dnorm( 178 , 20 ) ,
    log_sigma ~ dnorm( 2 , 10 )
  ) , data=d2 )
precis(m4.1_logsigma)

post <- extract.samples( m4.1_logsigma )
sigma <- exp( post$log_sigma )
