library(tidyverse)

read_csv("eval/zokrates/all_results.csv") %>% pivot_wider(names_from=compiler, values_from=c(constraints, wall_time)) %>% write_csv("eval/zokrates/all_wide.csv")
