#==============================================================================
# purpose: analyze stackoverflow data
# author: tirthankar chakravarty
# created: 
# revised: 
# comments: 
# - what is the daily number of questions that are asked on SO?
# - what is the proportion of those questions that are still open
#   clearly, there will be a bias here that older questions are more likely to 
#   to be closed. 
# - what is the time of day (minute by minute) that a question is most likely to 
#   have the longest time to first answer, in the R label?
# - what is the time of day (minute by minute) that a question is most likely 
#   to have the longest time to fist comment, in the R label?
# - Note that PostType = 1 is a question and PostType = 2 is an answer. 
#==============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(scales)

# read in the daily count and plot it
df_so = read_csv(file = "Data/DailySOCount.csv")
df_so %>% 
  mutate(CreationDateOnly = as.Date(CreationDateOnly)) %>% 
  ggplot(aes(x = CreationDateOnly, y = NumQuestions)) + 
  geom_line() + 
  theme_bw()

# highly periodic series 
df_so %>% 
  group_by(day_of_week = wday(CreationDateOnly, label = TRUE), 
           year = as.factor(year(CreationDateOnly))) %>% 
  summarize(NumQuestions = mean(NumQuestions)) %>% 
  ggplot(aes(x = year, y = NumQuestions, group = day_of_week, fill = day_of_week)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_bw()

#================================================
# 2. Closed versus not closed posts
#================================================
df_so_closed = read_csv(file = "Data/DailySOCountClosed.csv") %>% 
  mutate(CreationDate = as.Date(CreationDateOnly)) %>% 
  select(-CreationDateOnly) %>% 
  rename(all_posts = NumQuestions, 
         open_posts = count_nulls, 
         closed_posts = count_not_nulls)

df_so_closed %>% 
  gather(key = post_type, value = num_posts, -CreationDate) %>% 
  ggplot(aes(x = CreationDate, y = num_posts, color = post_type)) + 
  geom_line() + 
  facet_wrap(~ post_type, scales = "free_y", nrow = 3) + 
  theme_bw()
             
# aggregate up to the weekly level
df_so_closed %>% 
  group_by(week = week(CreationDate), year = year(CreationDate)) %>% 
  summarise_each(funs(sum), all_posts:closed_posts) %>% 
  arrange(week, year)

# format the dates into week-year
df_so_closed %>% 
  mutate(weekdate = floor_date(CreationDate, unit = "week")) %>% 
  group_by(weekdate) %>% 
  summarise_each(funs(sum), all_posts:closed_posts) %>% 
  arrange(weekdate) %>% 
  gather(key = post_type, value = post_count, -weekdate) %>% 
  ggplot(aes(x = weekdate, y = post_count)) + 
  geom_line() + 
  theme_bw() + 
  facet_wrap(~ post_type, scales = "free_y", nrow = 3)

#================================================
# 3. Closed versus not closed questions
#================================================
df_so_closed = read_csv(file = "Data/DailySOCountQuestions.csv") %>% 
  mutate(CreationDate = as.Date(CreationDateOnly)) %>% 
  select(-CreationDateOnly) %>% 
  rename(all_questions = NumQuestions, 
         open_questions = count_nulls, 
         closed_questions = count_not_nulls)

df_so_closed %>% 
  gather(key = question_type, value = num_questions, -CreationDate) %>% 
  ggplot(aes(x = CreationDate, y = num_questions, color = question_type)) + 
  geom_line() + 
  facet_wrap(~ question_type, scales = "free_y", nrow = 3) + 
  theme_bw()

# aggregate up to the weekly level
df_so_closed %>% 
  group_by(week = week(CreationDate), year = year(CreationDate)) %>% 
  summarise_each(funs(sum), question_count:closed_questions) %>% 
  arrange(week, year)

# format the dates into week-year
df_so_closed %>% 
  mutate(weekdate = floor_date(CreationDate, unit = "week")) %>% 
  group_by(weekdate) %>% 
  summarise_each(funs(sum), all_questions:closed_questions) %>% 
  arrange(weekdate) %>% 
  gather(key = question_type, value = question_count, -weekdate) %>% 
  ggplot(aes(x = weekdate, y = question_count)) + 
  geom_line() + 
  theme_bw() + 
  facet_wrap(~ question_type, scales = "free_y", nrow = 3)

#================================================
# 4. Closed versus not closed questions: R
#================================================
df_so_closed = read_csv(file = "Data/DailySOCountQuestionsR.csv") %>% 
  mutate(CreationDate = as.Date(CreationDateOnly)) %>% 
  select(-CreationDateOnly) %>% 
  rename(all_questions = NumQuestions, 
         open_questions = count_nulls, 
         closed_questions = count_not_nulls)

df_so_closed %>% 
  gather(key = question_type, value = num_questions, -CreationDate) %>% 
  ggplot(aes(x = CreationDate, y = num_questions, color = question_type)) + 
  geom_line() + 
  facet_wrap(~ question_type, scales = "free_y", nrow = 3) + 
  theme_bw()

# aggregate up to the weekly level
df_so_closed %>% 
  group_by(week = week(CreationDate), year = year(CreationDate)) %>% 
  summarise_each(funs(sum), question_count:closed_questions) %>% 
  arrange(week, year)

# format the dates into week-year
df_so_closed %>% 
  mutate(weekdate = floor_date(CreationDate, unit = "week")) %>% 
  group_by(weekdate) %>% 
  summarise_each(funs(sum), all_questions:closed_questions) %>% 
  arrange(weekdate) %>% 
  gather(key = question_type, value = question_count, -weekdate) %>% 
  ggplot(aes(x = weekdate, y = question_count)) + 
  geom_line() + 
  theme_bw() + 
  facet_wrap(~ question_type, scales = "free_y", nrow = 3)

# -- select count(*) as NumQuestions, cast(CreationDate as Date) as CreationDateOnly from Posts group by cast(CreationDate as Date) order by CreationDateOnly  
# -- select count(*) as NumQuestions, sum(case when ClosedDate is null then 1 else 0 end) as count_nulls, count(ClosedDate) as count_not_nulls, cast(CreationDate as Date)  CreationDateOnly from Posts where PostTypeId = 1 and Tags = '<r>' group by cast(CreationDate as Date) order by CreationDateOnly  
# -- select count(*) as NumQuestions, sum(case when ClosedDate is null then 1 else 0 end) as count_nulls, count(ClosedDate) as count_not_nulls, cast(CreationDate as Date)  CreationDateOnly from Posts where PosType = 2 group by cast(CreationDate as Date) order by CreationDateOnly  
# -- select * from PostTypes
# 
# select * from Posts where PostTypeId = 1 and Tags like '<r>'
# and month(CreationDate) = 1 and year(CreationDate) = 2015 and 
# day(CreationDate) = 1
# 
