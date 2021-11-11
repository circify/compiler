library(tidyverse)

d <- read_csv("eval/pequin/all_results.csv")
print(d)
dd <- d %>% pivot_wider(names_from=compiler, values_from=c(constraints, wall_time))
print(dd)
dd %>% write_csv("eval/pequin/all_wide.csv")
