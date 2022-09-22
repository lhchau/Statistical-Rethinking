library(rethinking)

set.seed(17)

N <- 2000

G <- sample(1:2, size = N, replace = TRUE)
u <- rbern(N, 0.1)

D <- rbern(N, ifelse(G == 1, u * 0.5, 0.8)) + 1

accept_rate_u0 <- matrix(c(0.1, 0.1, 0.1, 0.3), nrow = 2)
accept_rate_u1 <- matrix(c(0.2, 0.3, 0.2, 0.5), nrow = 2)

p <- sapply(1:N, function(i)
  ifelse(u[i] == 0, accept_rate_u0[D[i], G[i]],
         accept_rate_u1[D[i], G[i]]))
A <- rbern(N, p)

dat_sim <- list(A =  A, D = D, G = G)

# total effect gender
m1 <- ulam(
  alist(
    A ~ bernoulli(p),
    logit(p) <- a[G],
    a[G] ~ normal(0, 1)
  ), data = dat_sim, chains = 4, cores = 4
)

# direct effects - now confounded!
m2 <- ulam(
  alist(
    A ~ bernoulli(p),
    logit(p) <- a[G, D],
    matrix[G, D]:a ~ normal(0, 1)
  ), data = dat_sim, chains = 4, cores = 4
)

post2 <- extract.samples(m2)

post2$fm_contrast_D1 <- post2$a[, 1, 1] - post2$a[, 2, 1]
post2$fm_contrast_D2 <- post2$a[, 1, 2] - post2$a[, 2, 2]

# if we could measure u
dat_sim$u <- u
m3 <- ulam(
  alist(
    A ~ bernoulli(p),
    logit(p) <- a[G, D] + buA*u,
    matrix[G, D]:a ~ normal(0, 1),
    buA ~ normal(0, 1)
  ), data = dat_sim, constraints = list(buA = "lower=0"), chains = 4, cores = 4
)

post3 <- extract.samples(m3)
post3$fm_contrast_D1 <- post3$a[, 1, 1] - post3$a[, 2, 1]
post3$fm_contrast_D2 <- post3$a[, 1, 2] - post3$a[, 2, 2]

T1 <- rnorm(N, u, 0.1)
T2 <- rnorm(N, u, 0.5)
T3 <- rnorm(N, u, 0.25)

m4 <- ulam(
  alist(
    # A model
    A ~ bernoulli(p),
    logit(p) <- a[G, D] + b*u[i],
    matrix[G, D]:a ~ normal(0, 1),
    b ~ normal(0, 1),
    
    # u and T model
    vector[N]:u ~ normal(0, 1),
    T1 ~ normal(u, tau[1]),
    T2 ~ normal(u, tau[2]),
    T3 ~ normal(u, tau[3]),
    vector[3]:tau ~ exponential(1)
  ), data = dat_sim, chains = 4, cores = 4, constraints = list(b = "lower=0")
)
