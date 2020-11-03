library(tidyverse)
library(patchwork)

d <- read_csv("results.csv") %>% mutate(optimized = ifelse(optimized, "Optimized", "Unoptimized"))


t <-   theme(text = element_text(size=8),
                legend.key.size = unit(2,"mm"),
             legend.position = "bottom",
                axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
)

smt_plot <-
  ggplot(d) +
  geom_point(aes(x = size, y = 1000*z3_time, color = optimized, shape = optimized)) +
  labs(y = "Solver Time (ms)", shape = "Optimization:", color = "Optimization:") +
  t
ps_plot <-
  ggplot(d) +
  geom_point(aes(x = size, y = constraints/1000, color = optimized, shape=optimized)) +
  labs(y = "R1CS Constraints (k)", shape = "Optimization:", color = "Optimization:") +
  t
((smt_plot | ps_plot) / guide_area()) + plot_layout(heights = c(5,1),guides = 'collect')
ggsave("results.png", width = 3, height = 2, units = "in")
