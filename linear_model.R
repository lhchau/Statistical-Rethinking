library(rethinking)

pos_binom <- replicate(1000, sum(sample(x = 0:1, size = 16, replace = TRUE)))
hist(pos_binom)
plot(density(pos_binom))

pos_uniform <- replicate(1000, sum(runif(n = 16, min = 0, max = 1)))
hist(pos_uniform)
plot(density(pos_uniform))

pos_beta <- replicate(1000, sum(rbeta(n = 16, shape1 = 5, shape2 = 5)))
hist(pos_beta)
plot(density(pos_beta))

pos_chisq <- replicate(1000, sum(rchisq(n = 16, df = 5)))
hist(pos_chisq)
plot(density(pos_chisq))

prod( 1 + runif(12,0,0.1) )

growth <- replicate( 10000 , prod( 1 + runif(12,0,0.1) ) )
dens( growth , norm.comp=TRUE )

big <- replicate( 10000 , prod( 1 + runif(12,0,0.5) ) )
small <- replicate( 10000 , prod( 1 + runif(12,0,0.01) ) )

dens( log(big) , norm.comp=TRUE )
dens( small , norm.comp=TRUE )

# w ~ Binomial(n, p)
# p ~ Uniform(0, 1)
w <- 6; n <- 9;
p_grid <- seq(from=0,to=1,length.out=100)
posterior <- dbinom(w,n,p_grid)*dunif(p_grid,0,1) # likelihood * prior
posterior <- posterior/sum(posterior)

data("Howell1")
d <- Howell1
dens(d$height)

curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )

sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

d2 <- d[ d$age >= 18 , ]


mu.list <- seq( from=140, to=160 , length.out=200 )
sigma.list <- seq( from=4 , to=9 , length.out=200 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum( dnorm(
  d2$height ,
  mean=post$mu[i] ,
  sd=post$sigma[i] ,
  log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )

contour_xyz( post$mu , post$sigma , post$prob )
image_xyz( post$mu , post$sigma , post$prob )

plot(d2$height ~ d2$weight)

sample.rows <- sample(1:nrow(post), size = 1e4, replace = TRUE, 
                      prob = post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

plot(sample.mu, sample.sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.1))

dens(sample.mu)
dens(sample.sigma)

HPDI( sample.mu )
HPDI( sample.sigma )

d3 <- sample( d2$height , size=20 )

mu.list <- seq(from = 150, to = 170, length.out = 200)
sigma.list <- seq(from = 4, to = 20, length.out = 200)
post2 <- expand.grid(mu = mu.list, sigma = sigma.list)
post2$LL <- sapply(1:nrow(post2), function(i) 
  sum(dnorm(d3, mean = post2$mu[i], sd = post2$sigma[i],
            log = TRUE)))
post2$prod <- post2$LL + dnorm(post2$mu, 178, 20, TRUE) + dunif(post$sigma, 0, 50, TRUE)
post2$prob <- exp(post2$prod - max(post2$prod))
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE , prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu , sample2.sigma , cex=0.5 , col=col.alpha(rangi2,0.1) , 
      xlab="mu" , ylab="sigma" , pch=16 )

dens( sample2.sigma , norm.comp=TRUE )
# I do not understand why sigma has log normal distribution

library(rethinking)
library(tidyverse)

data("Howell1")
d <- Howell1
d <- d %>% 
  filter(age >= 18)

dat <- list(
  W = d$weight,
  S = d$male + 1
)

m_SW <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a[S],
    a[S] ~ dnorm(60, 10),
    sigma ~ dunif(0, 10)
  ), data = dat
)

precis(m_SW, depth = 2)

# posterior mean W
post <- extract.samples(m_SW)
dens(post$a[, 1], xlim = c(39, 50), lwd = 3, col = 2, xlab = "posterior mean weight (kg)")
dens(post$a[, 2], lwd = 3, col = 4, add = TRUE)

# posterior W distributions
W1 <- rnorm(1000, post$a[ ,1], post$sigma)
W2 <- rnorm(1000, post$a[ ,2], post$sigma)
dens(W1, xlim = c(20, 70), ylim = c(0, 0.085), col = 2, lwd = 3)
dens(W2, lwd = 3, col = 4, add = TRUE)

mu_contrast <- post$a[, 2] - post$a[, 1]
dens(mu_contrast, xlim = c(3, 10), lwd = 3, col = 1, xlab = "posterior mean weight constrast (kg)")

W1 <- rnorm(1000, post$a[, 1], post$sigma)
W2 <- rnorm(1000, post$a[, 2], post$sigma)

#contrast
W_contrast <- W2 - W1
dens(W_contrast, xlim = c(-25, 35), lwd = 3, col = 1, xlab = "posterior weight constrast (kg)")

mean(W_contrast > 0)

W_contrast %>% 
  as_tibble() %>% 
  ggplot(aes(x = value)) +
  geom_density(fill = "blue", color = "transparent")

xseq <- seq(from = 130, to = 190, len = 50)

data(WaffleDivorce)
d <- list()
d$A <- standardize( WaffleDivorce$MedianAgeMarriage )
d$D <- standardize( WaffleDivorce$Divorce )
d$M <- standardize( WaffleDivorce$Marriage )
m5.3_A <- quap(
  alist(
    ## A -> D <- M
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 ),
    ## A -> M
    M ~ dnorm( mu_M , sigma_M ),
    mu_M <- aM + bAM*A,
    aM ~ dnorm( 0 , 0.2 ),
    bAM ~ dnorm( 0 , 0.5 ),
    sigma_M ~ dexp( 1 )
  ) , data = d )

A_seq <- seq( from=-2 , to=2 , length.out=30 )

# prep data
sim_dat <- data.frame( A=A_seq )
# simulate M and then D, using A_seq
s <- sim( m5.3_A , data=sim_dat , vars=c("M","D") )

plot( sim_dat$A , colMeans(s$D) , ylim=c(-2,2) , type="l" ,
      xlab="manipulated A" , ylab="counterfactual D" )
shade( apply(s$D,2,PI) , sim_dat$A )
mtext( "Total counterfactual effect of A on D" )

# new data frame, standardized to mean 26.1 and std dev 1.24
sim2_dat <- data.frame( A=(c(20,30)-26.1)/1.24 )
s2 <- sim( m5.3_A , data=sim2_dat , vars=c("M","D") )
mean( s2$D[,2] - s2$D[,1] )

sim_dat <- data.frame( M=seq(from=-2,to=2,length.out=30) , A=0 )
s <- sim( m5.3_A , data=sim_dat , vars="D" )
plot( sim_dat$M , colMeans(s) , ylim=c(-2,2) , type="l" ,
      xlab="manipulated M" , ylab="counterfactual D" )
shade( apply(s,2,PI) , sim_dat$M )
mtext( "Total counterfactual effect of M on D" )


library(discrim)

parabolic_grid <-
  expand.grid(X1 = seq(-5, 5, length = 100),
              X2 = seq(-5, 5, length = 100))

fda_mod <-
  discrim_flexible(num_terms = 10) %>%
  # increase `num_terms` to find smoother boundaries
  set_engine("earth") %>%
  fit(class ~ ., data = parabolic)

parabolic_grid$fda <-
  predict(fda_mod, parabolic_grid, type = "prob")$.pred_Class1

mean(parabolic_grid$fda >= 0.5)
# num_terms = 3 -> Class 1 account for 61%
# num_terms = 2 -> CLass 1 account for 42%

library(ggplot2)
ggplot(parabolic, aes(x = X1, y = X2)) +
  geom_point(aes(col = class), alpha = .5) +
  geom_contour(data = parabolic_grid, aes(z = fda), 
               col = "black", breaks = .5) +
  theme_bw() +
  theme(legend.position = "top") +
  coord_equal()

# Full luxury Bayes
Hbar <- mean(d$height)
m_SHW_full <- quap(
  alist(
    # weight
    W ~ dnorm(mu, sigma),
    mu <- a[S] + b[S]*(H - Hbar),
    a[S] ~ dnorm(60, 10),
    b[S] ~ dlnorm(0, 1),
    sigma ~ dunif(0, 10),
    
    # height
    H ~ dnorm(nu, tau),
    nu <- H[S],
    H[S] ~ dnorm(160, 10),
    tau ~ dunif(0, 10)
  ), data = dat
)
























