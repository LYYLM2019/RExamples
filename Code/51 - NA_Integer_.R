library(dplyr)
set.seed(2)

df = data.frame(
  Group = factor(c(rep("A",3), rep(NA,3), rep("B",5), rep(NA,2))),
  Value = abs(as.integer(rnorm(13)*10))
) %>% 
  group_by(Group) %>%
  mutate(Diff = ifelse(is.na(Group), as.integer(NA), Value-lead(Value)))

