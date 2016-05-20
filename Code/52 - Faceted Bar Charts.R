library(readr)
library(dplyr)
library(tidyr)

df_foo = read.table(textConnection(
  "T6     T26   D6     D26
ENSMUSG00000026427    420     170   197    249
ENSMUSG00000026436     27     21    54      77
ENSMUSG00000018189    513    246   429    484
ENSMUSG00000026470    100     55    82     73
ENSMUSG00000026696    147     73   182    283
ENSMUSG00000026568   3620   1571  1264   1746
ENSMUSG00000026504     95     60   569    428"
))

# plot the data
df_foo %>% 
  add_rownames(var = "ID") %>% 
  gather(key = Variable, value = Value, -ID) %>% 
  ggplot(aes(x = ID, y = Value, fill = Variable)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  facet_wrap(~ Variable, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

# save the plot
ggsave("results/facted_bar.png", dpi = 600)