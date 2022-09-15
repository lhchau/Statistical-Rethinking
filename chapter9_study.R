library(rethinking)
library(tidyverse)
library(brms)

N <- 1000
G <- sample(1:2, size = N, replace = TRUE)
# gender 1 tends to apply to  department 1 to department 2
D <- rbern(N, ifelse(G == 1, 0.3, 0.8)) + 1
# matrix of acceptance rates [dept, gender]
accept_rate <- matrix(c(0.1, 0.3, 0.1, 0.3), nrow = 2)
# simulate acceptance
A <- rbern(N, accept_rate[D, G])

table(G, A)

df <- tibble(G = G,
             D = D,
             A = A)

total_G <- brm(A ~ 1 + G,
               data = df, family = gaussian,
               prior = c(prior(normal(0, 0.2), class = Intercept),
                         prior(normal(0, 0.4), class = b),
                         prior(exponential(1), class = sigma)),
               iter = 1000, warmup = 500, chains = 4, cores = 4, seed = 123)

direct_G <- brm(A ~ 1 + G + D,
                data = df, family = gaussian,
                prior = c(prior(normal(0, 0.2), class = Intercept),
                          prior(normal(0, 0.4), class = b),
                          prior(exponential(1), class = sigma)),
                iter = 1000, warmup = 500, chains = 4, cores = 4, seed = 123)

summary(total_G)
summary(direct_G)


a <- rnorm(1e4, 0, 0.2)
b <- rnorm(1e4, 0, 0.5)

xseq <- seq(from = -3, to = 3, len = 100)
p <- sapply(xseq, function(x) inv_logit(a+b*x))

plot(NULL, xlim = c(-2.5, 2.5), ylim = c(0, 1),
     xlab = "x value", ylab = "probability")


for(i in 1:10) lines(xseq, p[i, ], lwd = 3, col = 2)

##########################################################
N <- 1000
G <- sample(1:2, size = N, replace = TRUE)
# gender 1 tends to apply to  department 1 to department 2
D <- rbern(N, ifelse(G == 1, 0.3, 0.8)) + 1
# matrix of acceptance rates [dept, gender]
accept_rate <- matrix(c(0.05, 0.2, 0.1, 0.3), nrow = 2)
# simulate acceptance
A <- rbern(N, accept_rate[D, G])

table(G, A)

glm_total_G <- brm(
  A ~ 1 + G,
  data = df,
  family = bernoulli(link = "logit"),
  iter = 2000,
  warmup = 500,
  chains = 4,
  cores = 4,
  inits = "0",
  seed = 123
)

glm_direct_G <- brm(
  A ~ 1 + G + D,
  data = df,
  family = bernoulli(link = "logit"),
  iter = 2000,
  warmup = 500,
  chains = 4,
  cores = 4,
  inits = "0",
  seed = 123
)

summary(glm_total_G)
summary(glm_direct_G)

dat_sim <- list(A = A, D = D, G = G)

m1 <- ulam(
  alist(
    A ~ bernoulli(p),
    logit(p) <- a[G],
    a[G] ~ normal(0, 1)
  ), data = dat_sim, chains = 4, cores = 4
)

m2 <- ulam(
  alist(
    A ~ bernoulli(p),
    logit(p) <- a[G, D],
    matrix[G, D]:a ~ normal(0, 1)
  ), data = dat_sim, chains = 4, cores = 4
)

summary(m1)
summary(m2)
  
data("UCBadmit")
d <- UCBadmit

dat <- list(
  A = d$admit,
  N = d$applications,
  G = ifelse(d$applicant.gender=="female", 1, 2),
  D = as.integer(d$dept)
)

# total effect gender
mG <- ulam(
  alist(
    A ~ binomial(N, p),
    logit(p) <- a[G],
    a[G] ~ normal(0, 1)
  ), data = dat, chains = 4, cores = 4
)

# direct effects
mGD <- ulam(
  alist(
    A ~ binomial(N, p),
    logit(p) <- a[G, D],
    matrix[G, D]:a ~ normal(0, 1)
  ), data = dat, chains = 4, cores = 4
)

# Total effect
post1 <- extract.samples(mG)
PrA_G1 <- inv_logit(post1$a[, 1])
prA_G2 <- inv_logit(post1$a[, 2])
diff_prob <- PrA_G1 - prA_G2
dens(diff_prob, lwd = 4, col = 2, xlab = "Gender contrast (probability)")

# Direct effects
post2 <- extract.samples(mGD)
PrA <- inv_logit(post2$a)
diff_prob_D_ <- sapply(1:6, function(i) PrA[,1,i] - PrA[,2,i])

plot(NULL, xlim = c(-0.2, 0.3), ylim = c(0, 25), 
     xlab = "Gender constrast(probability)", ylab = "Density")
for(i in 1:6) dens(diff_prob_D_[, i], lwd = 4, col = 1+i, add = TRUE)
s

