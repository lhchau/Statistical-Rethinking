# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )

# define prior
prior <- rep( 1 , 20 )

# compute likelihood at each value in grid
likelihood <- dbinom( 6 , size=9 , prob=p_grid )

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )

prior <- ifelse( p_grid < 0.5 , 0 , 1 )
prior <- exp( -5*abs( p_grid - 0.5 ) )

library(rethinking)
globe.qa <- map(
  alist(
    w ~ dbinom(9,p) , # binomial likelihood
    p ~ dunif(0,1) # uniform prior
  ) ,
  data=list(w=6) )
# display summary of quadratic approximation
precis( globe.qa )

# analytical calculation
w <- 6
n <- 9
curve( dbeta( x , w+1 , n-w+1 ) , from=0 , to=1 )
# quadratic approximation
curve( dnorm( x , 0.67 , 0.16 ) , lty=2 , add=TRUE )

library(tidyverse)
library(patchwork)
library(rethinking)

# Comment attributes
# p_grid -> the grid approximation for model fit in.
# prior -> the probability for each value in p_grid can occur in real world
# likelihood -> the likelihood for the our observation occur under value p_grid
# posterior -> plausibility for each value in p_grid. It might be understood that the prior changed into posterior after having evidence.

(
  d <- 
    tibble(p_grid = seq(from = 0, to = 1, length.out = 20),
           prior = 1) %>% 
    mutate(likelihood = dbinom(x = 6, size = 9, prob = p_grid)) %>% 
    mutate(unstd_posterior = likelihood * prior) %>% 
    mutate(posterior = unstd_posterior / sum(unstd_posterior))
)

p1 <- 
  d %>% 
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "20 points grid approxmation",
       x = "probability of water",
       y = "posterior probability") +
  theme(panel.grid = element_blank())

p2 <-
  tibble(p_grid = seq(from = 0, to = 1, length.out = 5),
         prior = 1) %>% 
  mutate(likelihood = dbinom(x = 6, size = 9, prob = p_grid)) %>% 
  mutate(unstd_posterior = likelihood * prior) %>% 
  mutate(posterior = unstd_posterior / sum(unstd_posterior)) %>% 
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() +
  labs(subtitle = "5 points grid approximation",
       x = "probability of water",
       y = "posterior probability") +
  theme(panel.grid = element_blank())

p1 + p2 + plot_annotation(title = "More grid points make for smoother approximations")

globe_qa <- 
  rethinking::map(
    alist(
      w ~ dbinom(9, p),
      p ~ dunif(0, 1)
    ),
    data = list(w = 6)
  )

# display summary of quadratic approximation
precis(globe_qa)

globe_qa18 <- 
  rethinking::map(
    alist(
      w ~ dbinom(9 * 2, p),
      p ~ dunif(0, 1)
    ),
    data = list(w = 6 * 2)
  )

globe_qa_36 <-
  rethinking::map(
    alist(
      w ~ dbinom(9 * 4, p),
      p ~ dunif(0, 1)
    ), 
    data = list(w = 6 * 4)
  )

precis(globe_qa18)
precis(globe_qa_36)

n_grid <- 100

tibble(w = c(6, 12, 24),
       n = c(9, 18, 36),
       s = c(.16, .11, .08)) %>% # standard error
  expand(nesting(w, n, s),
         p_grid = seq(from = 0, to = 1, length.out = n_grid)) %>% 
  mutate(prior = 1,
         m     = .67) %>% 
  mutate(likelihood = dbinom(w, size = n, prob = p_grid)) %>% 
  mutate(unstd_grid_posterior = likelihood * prior,
         unstd_quad_posterior = dnorm(x = p_grid, mean = m, sd = s)) %>% 
  group_by(w) %>% 
  mutate(grid_posterior = unstd_grid_posterior / sum(unstd_grid_posterior),
         quad_posterior = unstd_quad_posterior / sum(unstd_quad_posterior),
         n = str_c("n = ", n)) %>% 
  mutate(n = factor(n, levels = c("n = 9", "n = 18", "n = 36"))) %>%  
  # plot
  ggplot(aes(x = p_grid)) +
  geom_line(aes(y = grid_posterior)) +
  geom_line(aes(y = quad_posterior),
            color = "grey50") +
  labs(x = "proportion water",
       y = "density") +
  theme(panel.grid = element_blank()) +
  facet_wrap(~n, scales = "free")


### 2.4.3 Markov chain Monte Carlo.
library(brms)

b2.1 <-
  brm(data = list(w = 24), 
      family = binomial(link = "identity"),
      w | trials(36) ~ 0 + Intercept,
      prior(beta(1, 1), class = b, lb = 0, ub = 1),
      seed = 2)

























