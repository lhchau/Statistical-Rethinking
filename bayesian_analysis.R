ways <- c(0, 3, 8, 9, 0)
ways/sum(ways)

new_draw <- 0:4
new_draw_ways <- ways*new_draw
new_draw_ways/sum(new_draw_ways)

plot(0:10 * 10:0)

L = 1000:0
W = 0:1000
LW = L * W
LWL = LW * L

plot(LW)
plot(LWL/sum(LWL))

plot(LWL)

set.seed(100)
arr = rbinom(50, 1, 0.7)
temp = rep(1, 1001)

for(i in seq_along(arr)){
  if(arr[[i]] == 1){
    temp <<- temp * W 
  }
  else{
    temp <<- temp * L
  }
}

plot(temp/sum(temp))

library(tidyverse)

d <- 
  tibble(
    p1 = 0,
    p2 = rep(1:0, times = c(1, 3)),
    p3 = rep(1:0, times = c(2, 2)),
    p4 = rep(1:0, times = c(3, 1)),
    p5 = 1
  )

d

d %>% 
  gather() %>% 
  mutate(x = rep(1:4, times = 5),
         possibility = rep(1:5, each = 4)) %>% 
  ggplot(aes(x = x, y = possibility,
             fill = value %>% as.character())) +
  geom_point(shape = 21, size = 5) +
  scale_fill_manual(values = c("white", "navy")) +
  scale_x_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = c(.75, 4.25),
                  ylim = c(.75, 5.25)) +
  theme(legend.position = "none")

tibble(draw = 1:3,
       marbles = 4) %>% 
  mutate(possibilities = marbles ^ draw) %>% 
  knitr::kable()

(
  d <- 
    tibble(position = c((1:4^1) / 4^0,
                        (1:4^2) / 4^1,
                        (1:4^3) / 4^2),
           draw     = rep(1:3, times = c(4^1, 4^2, 4^3)),
           fill     = rep(c("b", "w"), times = c(1, 3)) %>% 
             rep(., times = c(4^0 + 4^1 + 4^2))
           ) 
)

d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_point(aes(fill = fill),
             shape = 21, size = 3) +
  scale_y_continuous(breaks = 1:3) +
  scale_fill_manual(values = c("navy", "white")) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())

(
  lines_1 <- 
    tibble(
      x = rep((1:4), each = 4),
      xend = ((1:4^2) / 4),
      y = rep(1, times = 16),
      yend = rep(2, times = 16)
    )
)

(
  lines_2 <-
    tibble(
      x = rep(((1:4^2)/ 4), each = 4),
      xend = ((1:4^3) / 4^2),
      y = 2,
      yend = 3
    )
)

d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend)) +
  geom_segment(data = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend)) +
  geom_point(aes(fill = fill),
             shape = 21, size = 3) +
  scale_y_continuous(breaks = 1:3) +
  scale_fill_manual(values = c("navy", "white")) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())

d <-
  d %>% 
  mutate(denominator = ifelse(draw == 1, .5,
                              ifelse(draw == 2, .5 / 4,
                                     .5 / 4^2))) %>% 
  mutate(position = position - denominator)

(
  lines_1 <-
    lines_1 %>% 
    mutate(x = x - .5,
           xend = xend - .5 / 4)
)

(
  lines_2 <-
    lines_2 %>% 
    mutate(x = x - .5 / 4,
           xend = xend - .5 / 4^2)
)


d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size = 1/3) +
  geom_segment(data = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size = 1/3) +
  geom_point(aes(fill = fill),
             shape = 21, size = 3) +
  scale_y_continuous(breaks = 1:3) +
  scale_fill_manual(values = c("navy", "white")) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())

d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size = 1/3) +
  geom_segment(data = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               size = 1/3) +
  geom_point(aes(fill = fill),
             shape = 21, size = 4) +
  scale_fill_manual(values = c("navy", "white")) +
  scale_x_continuous(NULL, limits = c(0, 4), breaks = NULL) +
  scale_y_continuous(NULL, limits = c(0.75, 3), breaks = NULL) +
  coord_polar() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())


lines_1 <-
  lines_1 %>% 
  mutate(remain = c(rep(0:1, times = c(1, 3)),
                    rep(0, times = 4 * 3)))

lines_2 <-
  lines_2 %>% 
  mutate(remain = c(rep(0,   times = 4),
                    rep(1:0, times = c(1, 3)) %>% 
                      rep(., times = 3),
                    rep(0,   times = 12 * 4)))


d <-
  d %>% 
  mutate(remain = c(rep(1:0, times = c(1, 3)),
                    rep(0:1, times = c(1, 3)),
                    rep(0,   times = 4 * 4),
                    rep(1:0, times = c(1, 3)) %>% 
                      rep(., times = 3),
                    rep(0,   times = 12 * 4))) 

# finally, the plot:
d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend,
                   alpha = remain %>% as.character()),
               size = 1/3) +
  geom_segment(data = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend,
                   alpha = remain %>% as.character()),
               size = 1/3) +
  geom_point(aes(fill = fill, alpha = remain %>% as.character()),
             shape = 21, size = 4) +
  # it's the alpha parameter that makes elements semitransparent
  scale_alpha_manual(values = c(1/10, 1)) +
  scale_fill_manual(values = c("navy", "white")) +
  scale_x_continuous(NULL, limits = c(0, 4), breaks = NULL) +
  scale_y_continuous(NULL, limits = c(0.75, 3), breaks = NULL) +
  coord_polar() +
  theme(legend.position = "none",
        panel.grid = element_blank())

# if we make two custom functions, here, it will simplify the code within `mutate()`, below
n_blue <- function(x) {
  rowSums(x == "b")
}

n_white <- function(x) {
  rowSums(x == "w")
}

t <- 
  tibble(p_1 = rep(c("w", "b"), times = c(1, 4)),
         p_2 = rep(c("w", "b"), times = c(2, 3)),
         p_3 = rep(c("w", "b"), times = c(3, 2)),
         p_4 = rep(c("w", "b"), times = c(4, 1))) %>% 
  mutate(`draw 1: blue` = n_blue(.),
         `draw 2: white` = n_white(.),
         `draw 3: blue`  = n_blue(.)) %>% 
  mutate(`ways to produce` = `draw 1: blue` * `draw 2: white` * `draw 3: blue`)

t %>% 
  knitr::kable()

d <-
  tibble(position = c((1:4^1) / 4^0,
                      (1:4^2) / 4^1,
                      (1:4^3) / 4^2),
         draw     = rep(1:3, times = c(4^1, 4^2, 4^3)))



(
  d <-
    d %>% 
    bind_rows(
      d, d
    ) %>% 
    # here are the fill colors
    mutate(fill = c(rep(c("w", "b"), times = c(1, 3)) %>% rep(., times = c(4^0 + 4^1 + 4^2)),
                    rep(c("w", "b"), each  = 2)       %>% rep(., times = c(4^0 + 4^1 + 4^2)),
                    rep(c("w", "b"), times = c(3, 1)) %>% rep(., times = c(4^0 + 4^1 + 4^2)))) %>% 
    # now we need to shift the positions over in accordance with draw, like before
    mutate(denominator = ifelse(draw == 1, .5,
                                ifelse(draw == 2, .5 / 4,
                                       .5 / 4^2))) %>% 
    mutate(position = position - denominator) %>% 
    # here we'll add an index for which pie wedge we're working with
    mutate(pie_index = rep(letters[1:3], each = n()/3)) %>% 
    # to get the position axis correct for pie_index == "b" or "c", we'll need to offset
    mutate(position = ifelse(pie_index == "a", position,
                             ifelse(pie_index == "b", position + 4,
                                    position + 4 * 2)))
)

move_over <- function(position, index) {
  ifelse(index == "a", position,
         ifelse(index == "b", position + 4,
                position + 4 * 2)
  )
}

(
  lines_1 <-
    tibble(x    = rep((1:4), each = 4) %>% rep(., times = 3),
           xend = ((1:4^2) / 4)        %>% rep(., times = 3),
           y    = 1,
           yend = 2) %>% 
    mutate(x    = x - .5,
           xend = xend - .5 / 4^1) %>% 
    # here we'll add an index for which pie wedge we're working with
    mutate(pie_index = rep(letters[1:3], each = n()/3)) %>% 
    # to get the position axis correct for `pie_index == "b"` or `"c"`, we'll need to offset
    mutate(x    = move_over(position = x,    index = pie_index),
           xend = move_over(position = xend, index = pie_index))
)

(
  lines_2 <-
    tibble(x    = rep(((1:4^2) / 4), each = 4)  %>% rep(., times = 3),
           xend = (1:4^3 / 4^2)                 %>% rep(., times = 3),
           y    = 2,
           yend = 3) %>% 
    mutate(x    = x - .5 / 4^1,
           xend = xend - .5 / 4^2) %>% 
    # here we'll add an index for which pie wedge we're working with
    mutate(pie_index = rep(letters[1:3], each = n()/3)) %>% 
    # to get the position axis correct for `pie_index == "b"` or `"c"`, we'll need to offset
    mutate(x    = move_over(position = x,    index = pie_index),
           xend = move_over(position = xend, index = pie_index))
)

d <- 
  d %>% 
  mutate(remain = c(# `pie_index == "a"`
    rep(0:1, times = c(1, 3)),
    rep(0,   times = 4),
    rep(1:0, times = c(1, 3)) %>% 
      rep(., times = 3),
    rep(0,   times = 4 * 4),
    rep(c(0, 1, 0), times = c(1, 3, 4 * 3)) %>% 
      rep(., times = 3),
    # `pie_index == "b"`
    rep(0:1, each = 2),
    rep(0,   times = 4 * 2),
    rep(1:0, each = 2) %>% 
      rep(., times = 2),
    rep(0,   times = 4 * 4 * 2),
    rep(c(0, 1, 0, 1, 0), times = c(2, 2, 2, 2, 8)) %>% 
      rep(., times = 2),
    # `pie_index == "c"`
    rep(0:1, times = c(3, 1)),
    rep(0,   times = 4 * 3),
    rep(1:0, times = c(3, 1)), 
    rep(0,   times = 4 * 4 * 3),
    rep(0:1, times = c(3, 1)) %>% 
      rep(., times = 3),
    rep(0,   times = 4)
  )
  )

lines_1 <-
  lines_1 %>% 
  mutate(remain = c(rep(0,   times = 4),
                    rep(1:0, times = c(1, 3)) %>% 
                      rep(., times = 3),
                    rep(0,   times = 4 * 2),
                    rep(1:0, each  = 2) %>% 
                      rep(., times = 2),
                    rep(0,   times = 4 * 3),
                    rep(1:0, times = c(3, 1))
  )
  )

lines_2 <-
  lines_2 %>% 
  mutate(remain = c(rep(0,   times = 4 * 4),
                    rep(c(0, 1, 0), times = c(1, 3, 4 * 3)) %>% 
                      rep(., times = 3),
                    rep(0,   times = 4 * 8),
                    rep(c(0, 1, 0, 1, 0), times = c(2, 2, 2, 2, 8)) %>% 
                      rep(., times = 2),
                    rep(0,   times = 4 * 4 * 3),
                    rep(0:1, times = c(3, 1)) %>% 
                      rep(., times = 3),
                    rep(0,   times = 4)
  )
  )

d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_vline(xintercept = c(0, 4, 8), color = "white", size = 2/3) +
  geom_segment(data = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend,
                   alpha = remain %>% as.character()),
               size = 1/3) +
  geom_segment(data = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend,
                   alpha = remain %>% as.character()),
               size = 1/3) +
  geom_point(aes(fill = fill, size = draw, alpha = remain %>% as.character()),
             shape = 21) +
  scale_alpha_manual(values = c(1/10, 1)) +
  scale_fill_manual(values = c("navy", "white")) +
  scale_size_continuous(range = c(3, 1.5)) +
  scale_x_continuous(NULL, limits = c(0, 12),     breaks = NULL) +
  scale_y_continuous(NULL, limits = c(0.75, 3.5), breaks = NULL) +
  coord_polar() +
  theme(legend.position = "none",
        panel.grid = element_blank())

t <-
  t %>% 
  rename(`previous counts` = `ways to produce`,
         `ways to produce` = `draw 1: blue`) %>% 
  select(p_1:p_4, `ways to produce`, `previous counts`) %>% 
  mutate(`new count` = `ways to produce` * `previous counts`)

t %>% 
  knitr::kable()

t <-
  t %>% 
  select(p_1:p_4, `new count`) %>% 
  rename(`prior count` = `new count`) %>% 
  mutate(`factory count` = c(0, 3:0)) %>% 
  mutate(`new count` = `prior count` * `factory count`)

t %>% 
  knitr::kable()










