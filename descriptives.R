summary_stats <- df %>% 
  group_by(LowSugarFirst)%>%
  get_summary_stats(type = "common")

d %>% 
  filter(!is.na(Participation6)) %>%
  group_by(LowSugarFirst) %>%
  summarise(
    sum0 = sum(Participation6 == 0),
    sum1 = sum(Participation6 == 1))


df %>% freq_table(Age)


# create count table 
number_UK_return <- df_UK %>%
  filter(temporary_migr == 1 & year_return > 2008) %>%
  group_by(nationality_bac, year_return) %>%
  distinct() %>%
  count()%>%
  rename(returning = n)%>%
  rename(year = year_return)

# proportion table 
prop.table(table(df_before$highest_degree_new))

#summarise
df %>%
  filter(year_migr<2017, year_migr > 2013)%>%
  summarise(mean_age = mean(age_migr, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE),
            mean_stay = mean(months_stay, na.rm = TRUE),
            sd_stay = sd(months_stay, na.rm = TRUE))