library(tidyverse)
library(patchwork)

d <- read_csv("results.csv") %>% filter(size >= 2)
d$size = as_factor(d$size)

t <- theme(text = element_text(size=8),
           legend.key.size = unit(2,"mm"),
           legend.position = "bottom",
           axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
           )
s <- scale_y_continuous()

smt_plot <- ggplot(d, mapping = aes(x = 100 * largest_perm / perms, color=size, y = z3_time, shape = size)) +
  geom_point() +
  geom_line() +
  labs(y = "Solver Time (s)", x = "% Memory in Stack", color = "Permutation Size", shape = "Permutation Size") +
  s + t
ps_plot <- ggplot(d, mapping = aes(x = 100 * largest_perm / perms, color=size, y = constraints, shape = size)) +
  geom_point() +
  geom_line() +
  labs(y = "Constraints", x = "% Memory in Stack" , color = "Permutation Size", shape = "Permutation Size") +
  s + t

((smt_plot | ps_plot) / guide_area()) + plot_layout(heights = c(5,1),guides = 'collect')
ggsave("results.png", width = 3, height = 2, units = "in")
