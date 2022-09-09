library(tidyverse)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
C1.1.1,C1.1.2,C1.1.3,C1.1.4,C1.2.1,C1.2.2,C1.2.3,C1.2.4,C1.3.1,C1.3.2,C1.3.3,C1.3.4,C1.4.1,C1.4.2,C1.4.3,C1.4.4,C1.5.1,C1.5.2,C1.5.3,C1.5.4

data <- read_csv("data.csv")

data %>% 
  mutate(C1_new = rowMeans(select(data, starts_with("C1")))) %>% select(C1_new)

cor_data <- data %>% 
  mutate(
    C1_new = (C1.1.1+C1.1.2+C1.1.3+C1.1.4+C1.2.1+C1.2.2+C1.2.3+C1.2.4+C1.3.1+C1.3.2+C1.3.3+C1.3.4+C1.4.1+C1.4.2+C1.4.3+C1.4.4+C1.5.1+C1.5.2+C1.5.3+C1.5.4
)/20,
    C2_new = (C2.1.1+C2.1.2+C2.1.3+C2.1.4+C2.2.1+C2.2.2+C2.2.3+C2.2.4+C2.3.1+C2.3.2+C2.3.3+C2.3.4
)/12,
    C3_new = (C3.1.1+C3.1.2+C3.1.3+C3.1.4+C3.2.1+C3.2.2+C3.2.3+C3.2.4+C3.3.1+C3.3.2+C3.3.3+C3.3.4+C3.4.1+C3.4.2+C3.4.3+C3.4.4+C3.5.1+C3.5.2+C3.5.3+C3.5.4+C3.6.1+C3.6.2+C3.6.3+C3.6.4
)/24
  ) %>% 
  mutate(
    C1_diff = C1 - C1_new,
    C2_diff = C2 - C2_new,
    C3_diff = C3 - C3_new
  ) %>% 
  select(C1, C2, C3, C1_new, C2_new, C3_new, C1_diff, C2_diff, C3_diff)

diff_data <- data %>% 
  mutate(
    C1_new = (C3.1.1+C3.1.2+C3.1.3+C3.1.4)/4,
    C2_new = (C3.2.1+C3.2.2+C3.2.3+C3.2.4)/4,
    C3_new = (C3.3.1+C3.3.2+C3.3.3+C3.3.4)/4
  ) %>% 
  mutate(
    C1_diff = C1 - C1_new,
    C2_diff = C2 - C2_new,
    C3_diff = C3 - C3_new
  ) %>% 
  select(C1, C2, C3, C1_new, C2_new, C3_new, C1_diff, C2_diff, C3_diff)

cor_data <- data %>% 
  mutate(
    C1_new = (C3.1.1+C3.1.2+C3.1.3+C3.1.4)/4,
    C2_new = (C3.2.1+C3.2.2+C3.2.3+C3.2.4)/4,
    C3_new = (C3.3.1+C3.3.2+C3.3.3+C3.3.4)/4
  ) %>% 
  mutate(
    C1_diff = C1 - C1_new,
    C2_diff = C2 - C2_new,
    C3_diff = C3 - C3_new
  ) %>% 
  select(C1_new, C2_new, C3_new)

anova_data <- data %>% 
  mutate(
    C1_new = (C1.1.1+C1.1.2+C1.1.3+C1.1.4+C1.2.1+C1.2.2+C1.2.3+C1.2.4+C1.3.1+C1.3.2+C1.3.3+C1.3.4+C1.4.1+C1.4.2+C1.4.3+C1.4.4+C1.5.1+C1.5.2+C1.5.3+C1.5.4
    )/20,
    C2_new = (C2.1.1+C2.1.2+C2.1.3+C2.1.4+C2.2.1+C2.2.2+C2.2.3+C2.2.4+C2.3.1+C2.3.2+C2.3.3+C2.3.4
    )/12,
    C3_new = (C3.1.1+C3.1.2+C3.1.3+C3.1.4+C3.2.1+C3.2.2+C3.2.3+C3.2.4+C3.3.1+C3.3.2+C3.3.3+C3.3.4+C3.4.1+C3.4.2+C3.4.3+C3.4.4+C3.5.1+C3.5.2+C3.5.3+C3.5.4+C3.6.1+C3.6.2+C3.6.3+C3.6.4
    )/24
  ) %>% select(lop, gioitinh, hocluc, thaido, C1_new)

data.cor = cor(cor_data, method = "pearson")
cor_test_12 = cor.test(cor_data$C1_new, cor_data$C2_new, method = "pearson")
cor_test_13 = cor.test(cor_data$C1_new, cor_data$C3_new, method = "pearson")
cor_test_23 = cor.test(cor_data$C2_new, cor_data$C3_new, method = "pearson")

two_way <- aov(C1_new ~ lop + gioitinh + hocluc + thaido, data = anova_data)
summary(two_way)

two_way <- aov(C1_new ~ lop + gioitinh + hocluc + thaido, data = anova_data)
summary(two_way)

one_way_lop <- oneway.test(C1_new ~ lop, data = anova_data)
one_way_gioitinh <- oneway.test(C1_new ~ gioitinh, data = anova_data)
one_way_hocluc <- oneway.test(C1_new ~ hocluc, data = anova_data)
one_way_thaido <- oneway.test(C1_new ~ thaido, data = anova_data)

summary(one_way_lop)
summary(one_way_gioitinh)
summary(one_way_hocluc)
summary(one_way_thaido)

t_test_lop <- t.test(anova_data$C1_new, anova_data$lop)
t_test_gioitinh <- t.test(anova_data$C1_new, anova_data$gioitinh)
t_test_hocluc <- t.test(anova_data$C1_new, anova_data$hocluc)
t_test_thaido <- t.test(anova_data$C1_new, anova_data$thaido)





