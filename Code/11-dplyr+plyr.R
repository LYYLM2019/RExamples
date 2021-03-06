library(plyr)
library(dplyr)

ID=c(rep("ID1",3), rep("ID2",2), "ID3", rep("ID4",2))
item=c("a","b","c","a","c","a","b","a")

dfPaths = data.frame(ID, item)

dfPaths %>% group_by(ID) %>% filter(n()>1) %>% do(data.frame(V1=combn(as.character(.$item), 2, FUN=paste, collapse=";")))


dfPaths2 = dfPaths %>% 
  group_by(ID) %>% 
  mutate(numitems = n(), item = as.character(item)) %>%
  filter(numitems > 1)

  
ddply(dfPaths2, .(ID), function(x) t(combn(x$item, 2)))
