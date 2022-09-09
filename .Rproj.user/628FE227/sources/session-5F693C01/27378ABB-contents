library(tidyverse)
library(rethinking)
library(brms)
library(tidybayes)

theme_set(theme_light())

df <- read_csv("healthcare-dataset-stroke-data.csv") %>% 
  select(A = age, S = smoking_status, Y = stroke, X = heart_disease) 
str(df)

lm_YX <- lm(Y ~ X, data = df)
summary(lm_YX)

lm_YXSA <- lm(Y ~ X + S + A, data = df) 
summary(lm_YXSA)

df <- df %>% 
  mutate(S = case_when(
    S == "never smoked"    ~ 0,
    S == "smokes"          ~ 1,
    S == "formerly smoked" ~ 2,
    S == "Unknown"         ~ 3,
  ))

df$S <- df$S + 1

m6.13 <- quap(
  alist(
    Y      ~  dnorm(mu, sigma),
    mu     <- bX*X + a[S] + bA*A,
    bX     ~  dnorm(0, 0.5),
    a[S]   ~  dnorm(0, 1),
    bA     ~  dnorm(0, 0.5),
    sigma  ~  dexp(1)
  ), data = df
)

precis(m6.13, depth = 2)
