library(tidyverse)
library(patchwork)
source("../theme.R")

d <- read_csv("results.csv") %>% mutate(optimized = ifelse(optimized, "Optimized", "Unoptimized"))


t <-   t + theme(legend.position = "bottom")

smt_plot <-
  ggplot(d) +
  geom_point(aes(x = rounds, y = 1000*z3_time, color = optimized, shape = optimized)) +
  labs(y = "Solver Time (ms)", shape = "Optimization:", color = "Optimization:") +
  t
ps_plot <-
  ggplot(d) +
  geom_point(aes(x = rounds, y = constraints, color = optimized, shape=optimized)) +
  labs(y = "R1CS Constraints", shape = "Optimization:", color = "Optimization:") +
  scale_y_continuous(trans="log2") +
  ylim(0,8000) +
  t
((smt_plot | ps_plot) / guide_area()) + plot_layout(heights = c(5,1),guides = 'collect')
ggsave("results.png", width = 3, height = 2, units = "in")
