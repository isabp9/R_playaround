library(ggplot2)

d %>%
  group_by(LowSugarFirst) %>%
  ggplot(aes(x = Sports2, fill = LowSugarFirst)) +
  geom_bar(position = "dodge")+
  labs(x="Sports score in time = 50")

d %>% 
  select(order(colnames(d)))%>%
  group_by(LowSugarFirst) %>%
  select(Food1:Food9)%>%
  summarise(across(Food1:Food9, .f = list(mean = mean), na.rm = TRUE))%>%
  pivot_longer(cols = !LowSugarFirst, names_to ="Time", values_to = "Average_Food") %>%
  mutate(Time = parse_number(Time),Percent_Food = Average_Food * 100) %>%
  ggplot(aes(Time, Percent_Food, colour = factor(LowSugarFirst), group = factor(LowSugarFirst)))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept = 25, linetype = "dotted", size = 1.5)

# PLOT immigration to UK 
df_UK %>%
  filter(year_migr > 2013, year_migr < 2020) %>%
  group_by(nationality_bac, year_migr) %>% 
  summarise(count= n(), na.rm = TRUE) %>%
  ggplot(aes(x = year_migr, y= count))+
  geom_line(aes(color = nationality_bac, group = nationality_bac))+
  geom_point(aes(group = nationality_bac))+
  stat_summary(fun = mean, geom = "line", linewidth = 1)+  
  scale_x_continuous(breaks = seq(2014,2019,2))+
  labs(x= "Year", y = "n. Emigrants", color = "Nationality")


df %>%
  filter(age < 65, year_migr > 2013, year_migr < 2017)%>%
  ggplot( aes(x=age)) +
  geom_bar()+
  scale_x_continuous(breaks = seq(15,65,5))+
  labs(x= "Age", y = "n. users")

