library(dplyr)
library(tidyr)

dfX = read.table(
  textConnection("
issue_1  issue_2  issue_3  check  cat_1  cat_2  cat_3
a          -        -       0     1       0     0
-          b        -       1     0       1     0
-          -        c       1     0       0     1
p          -        -       0     1       0     0
-          -        q       1     0       0     1
-          r        -       0     0       1     0
a          -        -       1     1       0     0
a          b        -       1     1       1     0
                 "),
  header = TRUE, na.strings = "-", stringsAsFactors = FALSE)


dfX %>%
  group_by(issue_1) %>%
  summarize(total_issues = n(),
            check_again = sum(check),
            percentage = 100*(check_again/total_issues))

dfX %>%
  gather(issue_1, issue_2, issue_3)