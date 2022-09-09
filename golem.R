p_grid <- seq(from = 0, to = 1, length.out = 1000)
prob_p <- rep(1, 1000)
prob_data <- dbinom(6, size = 9, prob = p_grid)
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

plot(p_grid)
plot(prob_p)
plot(prob_data)
plot(posterior)

samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

plot(samples)
hist(samples)

w <- rbinom(1e4, size = 9, prob = samples)

hist(w)

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prob_p <- dbeta(p_grid, 3, 1)
prob_data <- dbinom(6, size = 9, prob = p_grid)
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

plot(p_grid)
plot(prob_p)
plot(prob_data)
plot(posterior)
