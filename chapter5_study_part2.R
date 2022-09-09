library(tidyverse)
library(rethinking)
library(brms)
library(tidybayes)

theme_set(theme_light())

### The Fork X <- Z -> Y
# X associated with Y because, X and Y inherit property (variance) of Z 
n <- 1000
Z <- rbern(n, 0.5)
X <- rbern(n, (1-Z)*0.1 + Z*0.9)
Y <- rbern(n, (1-Z)*0.1 + Z*0.9)

d_xyz <- tibble(X = X,
                Y = Y,
                Z = Z)

lm_XYZ <- lm(Y ~ X + Z, data = d_xyz)
lm_XY  <- lm(Y ~ X, data = d_xyz)

# When add Z into model we have an association between X and Y
summary(lm_XYZ)
# When remove Z away model we do not have an association between X and Y 
summary(lm_XY)

### The Pipe Bias or Post-treatment Bias
set.seed(71)
# number of plants
N <- 100

# simulate initial heights
h0 <- rnorm(N, 10, 2)

# assign treatments and simulate fungus and growth
treatment <- rep(0:1, each = N/2)
fungus <- rbinom(N, size = 1, prob = 0.5 - treatment*0.4)
h1 <- h0 +  rnorm(N, 5 - 3*fungus)

# H0 -> H1,
# Treatment -> Fungus
# Fungus -> H1
# indirect effect Treatment -> H1 ????

d <- data.frame(h0 = h0,
                h1 = h1,
                treatment = treatment,
                fungus = fungus)
precis(d)

sim_p <- rlnorm(1e4, 0, 0.25)
precis(data.frame(sim_p))

# the model for plant has fungus
m6.6 <- quap(
  alist(
    h1      ~  dnorm(mu, sigma),
    mu      <- h0*p,
    p       ~  dnorm(0, 0.25),
    sigma   ~  dexp(1)
  ), data = d
)

precis(m6.6)

#
m6.7 <- quap(
  alist(
    h1      ~  dnorm(mu, sigma),
    mu      <- h0 * p,
    p       <- a + bt*treatment + bf*fungus,
    a       ~ dlnorm(0, 0.2),
    bt      ~ dnorm(0, 0.5),
    bf      ~ dnorm(0, 0.5),
    sigma   ~ dexp(1)
  ), data = d
)
precis(m6.7)

m6.8 <- quap(
  alist(
    h1      ~  dnorm(mu, sigma),
    mu      <- h0 * p,
    p       <- a + bt*treatment,
    a       ~  dlnorm(0, 0.2),
    bt      ~  dnorm(0, 0.5),
    sigma   ~ dexp(1)
  ), data = d
)

precis(m6.8)

### The Collider Bias X -> Z <- Y

n <- 1000
X <- rbern(n, 0.5)
Y <- rbern(n, 0.5)
Z <- rbern(n, ifelse(X + Y > 0, 0.9, 0.2)) 

cols <- c(4, 2)

N    <- 300
X    <- rnorm(N)
Y    <- rnorm(N)
Z    <- rbern(N, inv_logit(2*X + 2*Y - 2))

plot(X, Y, col = cols[Z+1], lwd = 3)
abline(lm(Y[Z==1] ~ X[Z==1]), col = 2, lwd = 3)
abline(lm(Y[Z==0] ~ X[Z==0]), col = 4, lwd = 3)
abline(lm(Y ~ X), lwd = 3)

d_xyz <- tibble(X = X,
                Y = Y,
                Z = Z)

lm_XYZ <- lm(Y ~ X + Z, data = d_xyz)
lm_XY  <- lm(Y ~ X, data = d_xyz)

# When add Z into model we have an association between X and Y
summary(lm_XYZ)
# When remove Z away model we do not have an association between X and Y 
summary(lm_XY)

d <- sim_happiness(seed = 1997, N_years = 1000)
precis(d)

d2 <- d[d$age > 17,]
d2$A <- (d2$age - 18) / (65 - 18)

d2$mid <- d2$married + 1
d2$child <- rbern(n = length(d2$married), prob = ifelse(d2$married == 1, 0.95, 0.01))  

m6.9 <- quap(
  alist(
    happiness ~  dnorm(mu, sigma),
    mu        <- a[mid] + bA*A,
    a[mid]    ~  dnorm(0, 1),
    bA        ~  dnorm(0, 2),
    sigma     ~  dexp(1)
  ), data = d2
)

precis(m6.9, depth = 2)
# The model quite sure that **age** is negative association with **happiness** 

# Then we compare that model with the model omits the marriage status
m6.10 <- quap(
  alist(
    happiness ~  dnorm(mu, sigma),
    mu        <- a + bA*A,
    a         ~  dnorm(0, 1),
    bA        ~  dnorm(0, 2),
    sigma     ~  dexp(1)
  ), data = d2
)

precis(m6.10)

#########################################################
# Then we compare that model with the model omits the age 
m6.10.1 <- quap(
  alist(
    happiness ~  dnorm(mu, sigma),
    mu        <- a[mid],
    a[mid]    ~  dnorm(0, 1),
    sigma     ~  dexp(1)
  ), data = d2
)

precis(m6.10.1, depth = 2)

# Then we compare that model with the model omits the age 
m6.10.2 <- quap(
  alist(
    happiness ~  dnorm(mu, sigma),
    mu        <- a + a[mid],
    a         ~  dnorm(0, 1),
    a[mid]    ~  dnorm(0, 1),
    sigma     ~  dexp(1)
  ), data = d2
)

precis(m6.10.2, depth = 2)
#########################################################

### The collider's descendant
# The model includes child
d2$child <- d2$child + 1
m6.11 <- quap(
  alist(
    happiness ~  dnorm(mu, sigma),
    mu        <- a[child] + bA*A,
    a[child]  ~  dnorm(0, 1),
    bA        ~  dnorm(0, 2),
    sigma     ~  dexp(1)
  ), data = d2
)

precis(m6.11, depth = 2)
precis(m6.9, depth = 2)
precis(m6.10)

N <- 200
b_GP <- 1 # direct effect of G on P
b_GC <- 0 # direct effect of G on C
b_PC <- 1 # direct effect of P on C
b_U  <- 2 # direct effect of U on P and C

set.seed(1)
U <- 2*rbern(N, 0.5) - 1
G <- rnorm(N)
P <- rnorm(N, b_GP*G + b_U*U)
C <- rnorm(N, b_PC*P + b_GC*G + b_U*U)
d <- data.frame(C = C,
                P = P,
                G = G, 
                U = U)
m6.12 <- quap(
  alist(
    C ~ dnorm( mu , sigma ),
    mu <- a + b_PC*P + b_GC*G,
    a ~ dnorm( 0 , 1 ),
    c(b_PC,b_GC) ~ dnorm( 0 , 1 ),
    sigma ~ dexp( 1 )
  ), data=d )
precis(m6.12)

m6.13 <- quap(
  alist(
    C ~ dnorm( mu , sigma ),
    mu <- a + b_PC*P + b_GC*G + b_U*U,
    a ~ dnorm( 0 , 1 ),
    c(b_PC,b_GC,b_U) ~ dnorm( 0 , 1 ),
    sigma ~ dexp( 1 )
  ), data=d )
precis(m6.13)

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
precis(m5.3_A)

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





























