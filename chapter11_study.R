library(rethinking)

data(Trolley)
d <- Trolley

dat <- list(
  R = d$response,
  A = d$action,
  I = d$intention,
  C = d$contact
)

mRX <- ulam(
  alist(
    R ~ dordlogit(phi, alpha),
    phi <- bA*A + bI*I + bC*C,
    c(bA, bI, bC) ~ normal(0, 0.5),
    alpha ~ normal(0, 1)
  ), data = dat
)

precis(mRX, 2)

# plot predictive distributions for each treatment

vals <- c(0, 0, 0)
Rsim <- mcreplicate(100,
                    sim(mRX, data = list(A = vals[1], I = vals[2], C = vals[3])),
                    mc.cores = 6)

simplehist(as.vector(Rsim), lwd = 8, col = 2, xlab = "Response")
mtext(concat("A=", vals[1], ", I=", vals[2], ", C=", vals[3]))