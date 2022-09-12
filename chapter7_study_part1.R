library(tidyverse)
library(rcartocolor)

(
  d <- 
    tibble(species = c("afarensis", "africanus", "habilis", "boisei", "rudolfensis", "ergaster", "sapiens"), 
           brain   = c(438, 452, 612, 521, 752, 871, 1350), 
           mass    = c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5))
)

carto_pal(7, "BurgYl")
display_carto_pal(7, "BurgYl")

library(ggrepel)

d %>%
  ggplot(aes(x = mass, y = brain, label = species)) +
  geom_point(color = carto_pal(7, "BurgYl")) +
  geom_text_repel(size = 3, 
                  color = carto_pal(7, "BurgYl")[7],
                  family = "Courier",
                  seed = 438) +
  labs(subtitle = "Average brain volume by body\nmass for six hominin species",
       x = "body mass (kg)",
       y = "brain volume (cc)") +
  coord_cartesian(xlim = c(30, 65)) +
  theme_classic() +
  theme(text = element_text(family = "Courier"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1 /
                                                       4)))

library(broom)

fit_lm <- function(model, formula){
  model <- lm(formula = formula, data = d)
}

fits <- tibble(model = str_c("b6.", 1:6),
       formula = c("brain ~ mass",
                   "brain ~ mass + I(mass^2)",
                   "brain ~ mass + I(mass^2) + I(mass^3)",
                   "brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4)",
                   "brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5)",
                   "brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5) + I(mass^6)")) %>% 
  mutate(fit = map2(model, formula, fit_lm)) %>% 
  mutate(tidy = map(fit, tidy),
         glance = map(fit, glance))

fits <- fits %>% 
  mutate(r2 = glance %>% map_dbl("r.squared")) %>% 
  mutate(r2_text = round(r2, digits = 2) %>% as.character() %>% str_replace(., "0.", "."))

fits %>% 
  ggplot(aes(x = r2, y = formula, label = r2_text)) +
  geom_text(color = carto_pal(7, "BurgYl")[7], size = 3.5) +
  scale_x_continuous(expression(italic(R)^2), limits = 0:1, breaks = 0.1) +
  ylab(NULL) +
  theme_classic() +
  theme(text = element_text(family = "Courier"),
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4)))

fits %>% 
  unnest(tidy) %>% 
  select(model, term:estimate) %>% 
  mutate_if(is.double, round, digits = 1) %>% 
  complete(model, term) %>% 
  spread(key = term, value = estimate) %>% 
  select(model, `(Intercept)`, mass, everything())

p <-
  d %>% 
  ggplot(aes(x = mass, y = brain)) +
  geom_point(color = carto_pal(7, "BurgYl")[7]) +
  scale_x_continuous("body mass (kg)", limits = c(33, 62), expand = c(0, 0)) +
  ylab("brain volume (cc)") +
  coord_cartesian(ylim = c(300, 1500)) +
  theme_classic() +
  theme(text = element_text(family = "Courier"),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4)))

# linear
p1 <- 
  p +
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,  # note our rare use of 89% intervals
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,
              formula = y ~ x) +
  ggtitle(NULL, subtitle = expression(italic(R)^2==".49"))

# quadratic
p2 <-
  p + 
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,              
              formula = y ~ poly(x, 2)) +
  ggtitle(NULL, subtitle = expression(italic(R)^2==".54"))

# cubic
p3 <-
  p + 
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,              
              formula = y ~ poly(x, 3)) +
  ggtitle(NULL, subtitle = expression(italic(R)^2==".68"))

# fourth-order polynomial
p4 <-
  p + 
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,              
              formula = y ~ poly(x, 4)) +
  ggtitle(NULL, subtitle = expression(italic(R)^2==".81"))

# fifth-order polynomial
p5 <-
  p + 
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,              
              formula = y ~ poly(x, 5)) +
  # we're adjusting the y-axis range for this plot (and the next)
  coord_cartesian(ylim = c(150, 1900)) +
  ggtitle(NULL, subtitle = expression(italic(R)^2==".99"))

# sixth-order polynomial
p6 <-
  p + 
  # mark off 0 on the y-axis
  geom_hline(yintercept = 0, color = carto_pal(7, "BurgYl")[2], linetype = 2) + 
  stat_smooth(method = "lm", fullrange = TRUE, level = .89,
              color = carto_pal(7, "BurgYl")[6], fill = carto_pal(7, "BurgYl")[6], 
              size = 1/2, alpha = 1/3,
              formula = y ~ poly(x, 6)) +
  coord_cartesian(ylim = c(-300, 1500)) +
  ggtitle(NULL, subtitle = expression(italic(R)^2==1))

library(patchwork)
(p1 + p2) / (p3 + p4) / (p5 + p6)




