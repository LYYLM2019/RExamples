library(dplyr)
library(tidyr)
library(ggplot2)

# read data in
dfX = as_data_frame(read.table(textConnection("
                cust P1 P2 P3 P4 P5 P6 P7 P8 P9 P10 predict error
1     A  1  1  1  1  1  1  1  1  1   1       5     ?
               2     B  3  3  3  3  3  3  3  3  3   3       5     ?
               3     C  1  1  1  1  1  3  3  3  3   3       5     ?
               4     D  1  0  1  0  1  0  1  0  1   0       5     ?
               5     E  1  0  0  1  0  0  1  0  0   1       5     ?
               6     F  1  3  1  3  1  3  1  3  1   3       5     ?
               7     G  5  5  5  5  5  5  5  5  5   5       5     ?
               8     H  8  8  8  8  8  8  8  8  8   8       5     ?
               9     I  5  5  5  5  5  8  8  8  8   8       5     ?
               10    J  5  0  5  0  5  0  5  0  5   0       5     ?
               11    K  5  0  0  5  0  0  5  0  0   5       5     ?
               12    L  5  8  5  8  5  8  5  8  5   8       5     ?"),
                 header = TRUE, stringsAsFactors = FALSE))

# melt & compute error
dfXErr = dfX %>%
  select(-error) %>%
  gather(period, actual, -cust, -predict) %>%
  group_by(cust) %>%
  summarize(mape = mean(abs(actual - predict)))

# join back to original data
inner_join(dfX, dfXErr, by = "cust")
