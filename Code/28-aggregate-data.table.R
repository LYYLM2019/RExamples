library(dplyr)
# devtools::install_github("trinker/wakefield")
library(wakefield)

wakefield::r_data_frame(n = 1000,
  session_id = r_sample(x = 1:10),
  item_id = r_sample(x = 1:10)
) %>%
  dplyr::count(item_id, session_id)