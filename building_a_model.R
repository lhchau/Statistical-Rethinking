(d <- tibble(toss = c("w", "l", "w", "w", "w", "l", "w", "l", "w")))

(
  d <-
    d %>% 
    mutate(n_trials = 1:9,
           n_success = cumsum(toss == "w"))
)

sequence_length <- 50

d %>% 
  expand(nesting(n_trials, toss, n_success),
         p_water = seq(from = 0, to = 1, length.out = sequence_length)) %>% 
  group_by(p_water) %>% 
  mutate(lagged_n_trials = lag(n_trials, k = 1),
         lagged_n_success = lag(n_success, k = 1)) %>% 
  ungroup() %>% 
  mutate(prior = ifelse(n_trials == 1, .5,
                        dbinom(x = lagged_n_success,
                               size = lagged_n_trials,
                               prob = p_water)),
         likelihood = dbinom(x = n_success,
                             size = n_trials,
                             prob = p_water),
         strip = str_c("n = ", n_trials)) %>% 
  group_by(n_trials) %>% 
  mutate(prior = prior / sum(prior),
         likelihood = likelihood / sum(likelihood)) %>% 
  ggplot(aes(x = p_water)) +
  geom_line(aes(y = prior), linetype = 2) +
  geom_line(aes(y = likelihood)) +
  scale_x_continuous("proportion water", breaks = c(0, .5, 1)) +
  scale_y_continuous("plausibility", breaks = NULL) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~strip, scales = "free_y")

dbinom(x = 6, size = 9, prob = .5)

tibble(prob = seq(from = 0, to = 1, by = .01)) %>% 
  ggplot(aes(x = prob,
             y = dbinom(x = 6, size = 9, prob = prob))) +
  geom_line() +
  labs(x = "probability",
       y = "binomial likelihood") +
  theme(panel.grid = element_blank())

tibble(a = 0,
       b = c(1, 1.5, 2, 3, 9)) %>% 
  mutate(prob = 1 / (b - a))

tibble(a = 0,
       b = c(1, 1.5, 2, 3, 9)) %>% 
  expand(nesting(a, b), parameter_space = seq(from = 0, to = 9, length.out = 500)) %>% 
  mutate(prob = dunif(parameter_space, a, b),
         b = str_c("b = ", b)) %>% 
  ggplot(aes(x = parameter_space, ymin = 0, ymax = prob)) +
  geom_ribbon() +
  scale_x_continuous(breaks = c(0, 1:3, 9)) +
  scale_y_continuous(breaks = c(0, 1/9, 1/3, 1/2, 2/3, 1),
                     labels = c("0", "1/9", "1/3", "1/2", "2/3", "1")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  facet_wrap(~b, ncol = 5)

tibble(parameter_space = seq(from = 0, to = 1, length.out = 50)) %>% 
  # note our use of the `dbeta()` function
  mutate(prob = dbeta(parameter_space, 1, 1)) %>% 
  
  ggplot(aes(x = parameter_space, ymin = 0, ymax = prob)) +
  geom_ribbon() +
  ylim(0, 2) +
  theme(panel.grid = element_blank())


sequence_length <- 1e3

d <-
  tibble(probability = seq(from = 0, to = 1, length.out = sequence_length)) %>% 
  expand(probability, row = c("flat", "stepped", "Laplace")) %>% 
  arrange(row, probability) %>% 
  mutate(prior = ifelse(row == "flat", 1,
                        ifelse(row == "stepped", rep(0:1, each = sequence_length / 2),
                               exp(-abs(probability - .5) / .25) / (2 * .25))),
         likelihood = dbinom(x = 6, size = 9, prob = probability)) %>% 
  group_by(row) %>% 
  mutate(posterior = prior * likelihood / sum(prior * likelihood)) %>% 
  gather(key, value, -probability, -row) %>% 
  ungroup() %>% 
  mutate(key = factor(key, levels = c("prior", "likelihood", "posterior")),
         row = factor(row, levels = c("flat", "stepped", "Laplace"))) 

p1 <-
  d %>%
  filter(row == "flat") %>% 
  ggplot(aes(x = probability, y = value)) +
  geom_line() +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~key, scales = "free_y")

p2 <-
  d %>%
  filter(row == "stepped") %>% 
  ggplot(aes(x = probability, y = value)) +
  geom_line() +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  facet_wrap(~key, scales = "free_y")

p3 <-
  d %>%
  filter(row == "Laplace") %>% 
  ggplot(aes(x = probability, y = value)) +
  geom_line() +
  scale_x_continuous(NULL, breaks = c(0, .5, 1)) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  facet_wrap(~key, scales = "free_y")

library(patchwork)

p1 / p2 / p3
