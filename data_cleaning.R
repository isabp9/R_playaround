library(dplyr)
library(data.table)
library(tidyr) #pivot
library(collapse)
library(lubridate)
#read csv
library(readr)
#read xsl
library(readxl)
#read from folder 
library(arrow)

options(scipen=999)

##### READ DATA #####
## 1. read data xsl
path_in <- "C:/R_playaround/data.xsl"
df <- read_excel(path_in)

## 2. read from folder with arrow 
path_in <- "C:/R_playaround/data"
df_conn <- open_dataset(path_in)

# read in variables using dplyr
df <- df_conn %>%
  select(everything()) %>% #alternative to select few: select(c(country,time,year)) 
  collect()

# Transform to data.table
setDT(df)

#add id if needed
df <- df %>%
  mutate(id=row_number())


##### CLEAN DATA #####
# delete duplicates
df <- df[!duplicated(df)] 

#get size
length(unique(df$user_id)) 

# set variable value conditional on another var
df$var_a[df$var_b == 0] <- NA

df[ , var_a:=(ifelse(var_b == 1 & Time < 25, 1, 0))] #alternative using ifelse

#replace values bigger than 3 with a 3 for Eating score
df <- df %>%
  mutate( Eating1 = replace(Eating1, Eating1 > 3, 3))

#convert dates to dates values if character
df$date <- ymd(df$date)

#change data type
df <- df %>%
  mutate(across(where(is.character), as.numeric))

# CREATE NEW VARIABLES
#from dates create year variables 
df$year <- year(df$date)

# length of stay 
df$months_stay <- as.numeric(as.period(interval(df$date_migr, df$date_return)), "months")

# gender
df <- df %>%
  mutate(gender = ifelse(F_PROB > 0.9, "Female", "Male"))

# age and age clusters
df <- df %>%
  mutate(age_migr = age - (2023-year_migr))

df <- df %>%
  mutate(
    age_group_migr = case_when(
      age_migr > 17 & age_migr <= 24 ~ "18-24",
      age_migr > 24 & age_migr <= 30 ~ "25-30",
      age_migr > 30 & age_migr <= 35 ~ "31-35",
      age_migr > 34 & age_migr <= 40 ~ "35-40",
      age_migr > 40 & age_migr <= 45 ~ "41-45",
      age_migr > 44 & age_migr <= 50 ~ "45-50",
      age_migr > 50 & age_migr <= 55 ~ "51-55",
    )
  )




# restrict df to UK
df <- df[country == "UK",]

# drop one  column (return_UK)
df <- subset(df, select = -c(return_UK))


# rank n. users by nationality
rank_users <- df %>%
  filter(year_migr > 2013 & year_migr < 2020) %>%
  group_by(nationality_bac) %>% 
  summarise(count= n()) %>% 
  arrange(desc(count)) %>%
  slice(1:15) 

#rank_origin_countries.table <- xtable(rank_users)
#print(rank_origin_countries.table, include.rownames = FALSE)

# SELECT 9 NATIONALITIES
df<- df %>%
  filter(nationality_bac %in% 
           c("France", "Spain", "Germany", "Italy", "Switzerland", "Netherlands", "Belgium", "Greece","Portugal"))

#### PIVOT (wide) #####
us_rent_income %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  )


#### MELT #####

eating <- df %>% 
  select(order(colnames(df)))%>%
  rename("Eating50"="Eating2") %>% 
  select(c(Eating1, Eating50, id))%>%
  pivot_longer(cols = !c(id), names_to ="Time", values_to = "Eating") %>%
  mutate(Time = parse_number(Time))


#### JOIN ####
df <- attendance %>%
  left_join(sleep, by = c("id", "Time")) # inner or outer or right or full 

number_UK <- merge(number_UK_migrants, number_tot_users, by='nationality_bac')
