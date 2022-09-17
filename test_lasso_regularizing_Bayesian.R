library(rethinking)

data("WaffleDivorce")
d <- WaffleDivorce %>% as_tibble()

d$A <- standardize(d$MedianAgeMarriage)
d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)

m5.1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA*A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d)

m5.2 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d)

m5.3 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M + bA*A,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
), data = d)

precis(m5.1) 
precis(m5.2)
precis(m5.3)

compare(m5.1, m5.2, m5.3)

m5.3.1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M + bA*A,
    a ~ dnorm(0, 0.2),
    bM ~ dlaplace(0, 0.2),
    bA ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d)

m5.3.2 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M + bA*A,
    a ~ dnorm(0, 0.2),
    c(bM, bA) ~ dlaplace(0, 1),
    sigma ~ dexp(1)
  ), data = d)


precis(m5.3.1)

compare(m5.3.1, m5.3.2)

p1 <- extract.samples(m5.3.1) %>% 
  as_tibble() %>% 
  ggplot(aes(x = bM)) +
  geom_histogram() 

p2 <- extract.samples(m5.3.1) %>% 
  as_tibble() %>% 
  ggplot(aes(x = bA)) +
  geom_histogram() 

p3 <- extract.samples(m5.3.2) %>% 
  as_tibble() %>% 
  ggplot(aes(x = bM)) +
  geom_histogram() 

p4 <- extract.samples(m5.3.2) %>% 
  as_tibble() %>% 
  ggplot(aes(x = bA)) +
  geom_histogram() 

library(patchwork)

(p1 + p2) / (p3 + p4)




