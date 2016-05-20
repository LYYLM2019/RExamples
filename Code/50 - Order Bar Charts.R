library(ggplot2)
library(dplyr)
library(scales)

set.seed(145)

# simulate the data
df_foo = data.frame(Age=sample(c(1:10),20,replace=TRUE),
                 Rank=sample(c("Extremely","Very","Slightly","Not At All"),
                             20,replace=TRUE),
                 Percent=(runif(10,0,.01)))

# get the ordering that you are interested in
age_order = df_foo %>% 
  filter(Rank %in% c("Extremely", "Very")) %>% 
  group_by(Age) %>% 
  summarize(SumRank = sum(Percent)) %>% 
  arrange(desc(SumRank)) %>% 
  `[[`("Age")

# in some cases ages do not appear in the order because the 
#   ordering logic does not span all categories
age_order = c(age_order, setdiff(unique(df_foo$Age), age_order))
  
# make age a factor sorted by the ordering above  
ggplot(df_foo, aes(x = factor(Age, levels = age_order), y = Percent, fill = Rank))+
  geom_bar(stat = "identity") +
  coord_flip() + 
  theme_bw() + 
  scale_y_continuous(labels = percent) + 
  xlab("Age") + 
  ylab("Percentage")

# save the plot
ggsave("results/ordered_bar_chart.png", dpi = 600)
