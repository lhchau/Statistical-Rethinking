library(tidyverse)
library(rethinking)
library(brms)
library(tidybayes)

theme_set(theme_light())

set.seed(1914)
N <- 200 # num of grant proposals
p <- 0.1 # proportion to select
# uncorrelated newsworthiness and trustworthiness
nw <- rnorm(N)
tw <- rnorm(N)
# select top 10% of combined scores
s <- nw + tw # total score
q <- quantile(s, 1-p) # top 10% threshold
selected <- ifelse(s >= p, TRUE, FALSE)
cor( tw[selected], nw[selected] )

N <- 100 # number of individuals
set.seed(909)
height <- rnorm(N,10,2) # sim total height of each
leg_prop <- runif(N,0.4,0.5) # leg as proportion of height
leg_left <- leg_prop*height + # sim left leg as proportion + error
  rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height + # sim right leg as proportion + error
  rnorm( N , 0 , 0.02 )
# combine into data frame
d <- data.frame(height,leg_left,leg_right)

m6.1 <- brm(height ~ 1 + leg_left + leg_right,
            data = d, family = gaussian,
            prior = c(prior(normal(10, 100), class = Intercept),
                      prior(normal(2, 10), class = b, coef = leg_left),
                      prior(normal(2, 10), class = b, coef = leg_right),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 123)

m6.1_l <- brm(height ~ 1 + leg_left,
            data = d, family = gaussian,
            prior = c(prior(normal(10, 100), class = Intercept),
                      prior(normal(2, 10), class = b, coef = leg_left),
                      prior(exponential(1), class = sigma)),
            iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 123)

m6.1_r <- brm(height ~ 1 + leg_right,
              data = d, family = gaussian,
              prior = c(prior(normal(10, 100), class = Intercept),
                        prior(normal(2, 10), class = b, coef = leg_right),
                        prior(exponential(1), class = sigma)),
              iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 123)

summary(m6.1)

bind_rows(
  spread_draws(m6.1, `b_.*`, regex = TRUE) %>% 
    mutate(model = "mod_all"),
  spread_draws(m6.1_l, `b_.*`, regex = TRUE) %>% 
    mutate(model = "mod_l"),
  spread_draws(m6.1_r, `b_.*`, regex = TRUE) %>% 
    mutate(model = "mod_r")
) %>% 
  pivot_longer(starts_with("b_"),
               names_to = "parameter",
               values_to = "value",
               values_drop_na = TRUE) %>% 
  ggplot(aes(x = value, y = model)) +
  facet_wrap(~ parameter) +
  stat_halfeye(.width = 0.89) +
  labs(x = "Parameter Value",
       y = "Model")
  
spread_draws(m6.1, `b_.*`, regex = TRUE) %>% 
  pivot_longer(starts_with("b_"),
               names_to = "parameter",
               values_to = "value",
               values_drop_na = TRUE) %>% 
  ggplot(aes(x = value, y = parameter)) +
  stat_halfeye() +
  labs(x = "Parameter Value",
       y = "Parameter")


m6.1 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dexp( 1 )
  ) , data=d )
precis(m6.1)

plot(precis(m6.1))

post <- extract.samples(m6.1)
plot( bl ~ br , post , col=col.alpha(rangi2,0.1) , pch=16 )


data(milk)
d <- milk
d$K <- standardize( d$kcal.per.g )
d$F <- standardize( d$perc.fat )
d$L <- standardize( d$perc.lactose )

# kcal.per.g regressed on perc.fat
m6.3 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bF*F ,
    a ~ dnorm( 0 , 0.2 ) ,
    bF ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data=d )
# kcal.per.g regressed on perc.lactose
m6.4 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bL*L ,
    a ~ dnorm( 0 , 0.2 ) ,
    bL ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data=d )
precis( m6.3 )
precis( m6.4 )

m6.5 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bF*F + bL*L ,
    a ~ dnorm( 0 , 0.2 ) ,
    bF ~ dnorm( 0 , 0.5 ) ,
    bL ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) ,
  data=d )
precis( m6.5 )

milk_all <- brm(K ~ 1 + F + L, 
                data = d, family = gaussian,
                prior = c(prior(normal(0, 0.5), class = Intercept),
                          prior(normal(0, 1), class = b, coef = F),
                          prior(normal(0, 1), class = b, coef = L),
                          prior(exponential(1), class = sigma)),
                iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 123)

milk_fat <- brm(K ~ 1 + F,
                data = d, family = gaussian,
                prior = c(prior(normal(0, 0.7), class = Intercept),
                          prior(normal(0, 0.3), class = b, coef = F),
                          prior(exponential(1), class = sigma)),
                iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 123)

milk_lac <- brm(K ~ 1 + L,
                data = d, family = gaussian,
                prior = c(prior(normal(0, 0.3), class = Intercept),
                          prior(normal(0, 0.2), class = b, coef = L),
                          prior(exponential(1), class = sigma)),
                iter = 4000, warmup = 2000, chains = 4, cores = 4, seed = 123)

bind_rows(
  spread_draws(milk_fat, `b_.*`, regex = TRUE) %>% 
    mutate(model = "milk_fat"),
  spread_draws(milk_lac, `b_.*`, regex = TRUE) %>% 
    mutate(model = "milk_lac"),
  spread_draws(milk_all, `b_.*`, regex = TRUE) %>% 
    mutate(model = "milk_all")
) %>% 
  pivot_longer(starts_with("b_"), 
               names_to = "parameter",
               values_to = "value",
               values_drop_na = TRUE) %>% 
  mutate(model = factor(model, levels = c("milk_fat", "milk_lac", "milk_all"))) %>% 
  ggplot(aes(x = value, y = fct_rev(model))) +
  facet_wrap(~ parameter) +
  stat_halfeye(.width = 0.89) +
  labs(x = "Parameter Value",
       y = "Model")

milk_draw_post <- bind_rows(
  as_draws_df(milk_all) %>% 
    as_tibble() %>% 
    select(.draw, b_Intercept:sigma) %>% 
    mutate(model = "milk_all"),
  as_draws_df(milk_fat) %>% 
    as_tibble() %>% 
    select(.draw, b_Intercept:sigma) %>% 
    mutate(model = "milk_fat"),
  as_draws_df(milk_lac) %>% 
    as_tibble() %>% 
    select(.draw, b_Intercept:sigma) %>% 
    mutate(model = "milk_lac")
) %>% 
  pivot_longer(starts_with("b_"),
               names_to = "parameter",
               values_to = "value",
               values_drop_na = TRUE) %>% 
  mutate(model = factor(model, levels = c("milk_fat", "milk_lac", "milk_all"))) 

pairs( ~ kcal.per.g + perc.fat + perc.lactose , data=d , col=rangi2 )



sim.coll <- function( r=0.9 ) {
  d$x <- rnorm( nrow(d) , mean=r*d$perc.fat ,
                sd=sqrt( (1-r^2)*var(d$perc.fat) ) )
  m <- lm( kcal.per.g ~ perc.fat + x , data=d )
  sqrt( diag( vcov(m) ) )[2] # stddev of parameter
}

rep.sim.coll <- function( r=0.9 , n=100 ) {
  stddev <- replicate( n , sim.coll(r) )
  mean(stddev)
}

r.seq <- seq(from=0,to=0.99,by=0.01)
stddev <- sapply( r.seq , function(z) rep.sim.coll(r=z,n=100) )
plot( stddev ~ r.seq , type="l" , col=rangi2, lwd=2 , xlab="correlation" )

set.seed(71)
# number of plants
N <- 100
# simulate initial heights
h0 <- rnorm(N,10,2)
# assign treatments and simulate fungus and growth
treatment <- rep( 0:1 , each=N/2 )
fungus <- rbinom( N , size=1 , prob=0.5 - treatment*0.4 )
h1 <- h0 + rnorm(N, 5 - 3*fungus)
# compose a clean data frame
d <- data.frame( h0=h0 , h1=h1 , treatment=treatment , fungus=fungus )
precis(d)

m6.6 <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ),
    mu <- h0*p,
    p ~ dlnorm( 0 , 0.25 ),
    sigma ~ dexp( 1 )
  ), data=d )
precis(m6.6)

m6.7 <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ),
    mu <- h0 * p,
    p <- a + bt*treatment + bf*fungus,
    a ~ dlnorm( 0 , 0.2 ) ,
    bt ~ dnorm( 0 , 0.5 ),
    bf ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp( 1 )
  ), data=d )
precis(m6.7)

save.image("chapter6_study.Rdata")



















