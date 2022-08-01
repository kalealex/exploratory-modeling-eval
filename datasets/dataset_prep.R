library(readr)
library(dplyr)
## Preparing datasets for EVM

# forest fires:
# load
df_fires <- read_csv("forestfires.csv") 
# drop factors we don't want
df_fires <- df_fires %>%
  select(-c(X, Y, FFMC, DMC, ISI))
# # reorder factors
# df_fires <- df_fires %>%
#   mutate(
#     month = fct_relevel(month, "jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),
#     day = fct_relevel(day, "mon","tue","wed","thu","fri","sat","sun")
#   )
# make rain binary since it basically is already
df_fires <- df_fires %>%
  mutate(
    rain = if_else(rain > 0, "yes", "no")
  )
# rename factors that might be hard to interpret
df_fires <- df_fires %>%
  rename(
    drought_index = DC,
    temperature = temp,
    relative_humidity = RH,
    wind_speed = wind,
    area_burned = area
  )

# student absences:
# load math and language class data separately
df_mat <- read.table("student-mat.csv", sep=";", header=TRUE) %>% dplyr::select(-one_of(c("G1","G2"))) %>% mutate(topic = "math")
df_lan <- read.table("student-por.csv", sep=";", header=TRUE) %>% dplyr::select(-one_of(c("G1","G2"))) %>% mutate(topic = "language")
# join into one dataset
df_students <- df_mat %>% 
  full_join(df_lan, by = c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","guardian","traveltime","studytime","failures","schoolsup","famsup","paid","activities","nursery","higher","internet","romantic","famrel","freetime","goout", "Dalc","Walc","health","absences","G3","topic"))
# figure out which parent's education and job to use
df_students <- df_students %>%
  mutate(
    guardian_education = if_else(guardian == "father", Fedu, Medu),
    guardian_job = if_else(guardian == "father", Fjob, Mjob)
  )
# drop factors we don't want
df_students <- df_students %>%
  select(-c(school, sex, famsize, Pstatus, Medu, Fedu, Mjob, Fjob, reason, guardian, schoolsup, famsup, paid, activities, nursery, higher, internet, romantic, famrel, freetime, goout, Dalc, Walc, health, G3, topic))  
# recode address
df_students <- df_students %>%
  mutate(
    address = if_else(address=="U", "yes", "no")
  )
# add noise to travel time and study time to make them continuous
df_students <- df_students %>%
  rowwise() %>%
  mutate(
    traveltime = traveltime + runif(1, -0.99, 0.99),
    studytime = studytime + runif(1, -0.99, 0.99)
  )
# rename factors that might be hard to interpret
df_students <- df_students %>%
  rename(
    classes_failed = failures,
    urban_address = address,
    travel_time = traveltime,
    study_time = studytime 
  )
# match dataset size
df_students <- df_students %>% 
  ungroup() %>% 
  sample_n(nrow(df_fires))


# save version of both datasets to use in our evaluation
write_csv(df_fires, "eval_forestfires.csv")
write_csv(df_students, "eval_students.csv")

