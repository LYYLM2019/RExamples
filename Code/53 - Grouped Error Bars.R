# purpose: show how to color CIs by group
# author: tirthankar chakravarty
# created: 22nd march 2016
# revised: 

library(readr)
library(dplyr)
library(ggplot2)

# simulate some data
year_min = 1985
year_max = 2016
num_years = year_max - year_min + 1
num_groups = 2
num_estimates = num_years*num_groups

df_foo = data_frame(
  upper_limit = runif(n = num_estimates, min = -20, max = 20),
  lower_limit = upper_limit - runif(n = num_estimates, min = 0, max = 5),
  point_estimate = runif(num_estimates, min = lower_limit, max = upper_limit),
  year = rep(seq(year_min, year_max), num_groups),
  group = rep(c("mfin", "ffin"), each = num_years)
)

# plot the confidence intervals
df_foo %>% 
  ggplot(aes(x = year, y = point_estimate, 
             ymin = lower_limit, ymax = upper_limit,
             color = group)) + 
  geom_point() + 
  geom_errorbar() + 
  theme_bw() + 
  ylab("Odds Ratio & 95% CI") + 
  xlab("Year") + 
  scale_color_discrete(name = "Group")

# save the chart
ggsave("results/error_bar.png", dpi = 600)