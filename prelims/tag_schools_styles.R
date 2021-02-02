# tag.R

# Tagging schools where teachers use team teaching and group work styles

# Libraries
library(foreign)
library(dplyr)

Sys.setlocale("LC_CTYPE", "russian")

### SUS

setwd(paste0("C:/Users/", Sys.getenv("USERNAME"), '/YandexDisk/SUS_TIMSS/Data/SUS'))
tch_sus <- read.csv('export_teachers_flat.csv', sep = '\t') %>% 
  select(IDSCHOOL, IDTEACH, n24_1, n24_3)

# Teaching styles categories:
# 1 = Never;
# 2 = Once a month;
# 3 = Once a week;
# 4 = 2-4 times a week;
# 5 = Every day;

tch_sus$small_groups_tag <- ifelse(tch_sus$n24_1 >= 4, 1, 0)
tch_sus$team_teaching_tag <- ifelse(tch_sus$n24_3 >= 4, 1, 0)
tch_sus <- tch_sus %>% group_by(IDSCHOOL) %>% 
  mutate(n_tch = n(),
         n_tch_small_groups = sum(small_groups_tag),
         n_tch_team_teaching = sum(team_teaching_tag)) %>%
  filter(!duplicated(IDSCHOOL)) %>%
  mutate(pct_tch_small_groups = n_tch_small_groups/n_tch,
         pct_tch_team_teaching = n_tch_team_teaching/n_tch) %>%
  select(IDSCHOOL, n_tch, pct_tch_small_groups, pct_tch_team_teaching) %>%
  arrange(desc(pct_tch_small_groups, pct_tch_team_teaching))

tch_sus <- na.omit(tch_sus)
tch_sus[,c('pct_tch_small_groups', 'pct_tch_team_teaching')] <- round(tch_sus[,c('pct_tch_small_groups', 'pct_tch_team_teaching')], 2)

# Storing in .xlsx
writexl::write_xlsx(tch_sus, 'C:/Country/Russia/Data/SEASHELL/SEABYTE/edsus/out/sch_id_styles.xlsx')




