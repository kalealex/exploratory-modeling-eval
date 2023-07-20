library(readr)
library(dplyr)


## Preparing datasets for EVM

# forest fires: https://archive.ics.uci.edu/ml/datasets/forest+fires
# load
df_fires <- read_csv("forestfires.csv") 
# drop factors we don't want
df_fires <- df_fires %>%
  select(-c(X, Y, ISI))
# make rain binary since it basically is already
df_fires <- df_fires %>%
  mutate(
    rain = if_else(rain > 0, "yes", "no")
  )
# rename factors that might be hard to interpret
df_fires <- df_fires %>%
  rename(
    ffmc = FFMC,
    dmc = DMC,
    dc = DC,
    rh = RH
  )

# students: https://archive.ics.uci.edu/ml/datasets/student%2Bperformance
# load math and language class data separately
df_mat <- read.table("student-mat.csv", sep=";", header=TRUE) %>% dplyr::select(-one_of(c("G1","G2"))) %>% mutate(topic = "math")
df_lan <- read.table("student-por.csv", sep=";", header=TRUE) %>% dplyr::select(-one_of(c("G1","G2"))) %>% mutate(topic = "language")
# join into one dataset
df_students <- df_mat %>% 
  full_join(df_lan, by = c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","guardian","traveltime","studytime","failures","schoolsup","famsup","paid","activities","nursery","higher","internet","romantic","famrel","freetime","goout", "Dalc","Walc","health","absences","G3","topic"))
# figure out which parent's education and job to use
df_students <- df_students %>%
  mutate(
    g_edu = if_else(guardian == "father", Fedu, Medu),
    g_job = if_else(guardian == "father", Fjob, Mjob)
  )
# compute daily average alcohol consumption rating
df_students <- df_students %>%
  mutate(
    alcohol = (Dalc * 5.0 + Walc * 2.0) / 7.0
  )
# drop factors we don't want
df_students <- df_students %>%
  dplyr::select(-c(school, sex, famsize, Pstatus, Medu, Fedu, Mjob, Fjob, reason, guardian, schoolsup, famsup, paid, activities, nursery, higher, romantic, famrel, freetime, goout, Dalc, Walc, health, G3, topic))  
# recode address
df_students <- df_students %>%
  mutate(
    address = if_else(address=="U", "urban", "rural")
  )
# sample continuous values for travel time and study time based on dataset documentation
df_students <- df_students %>%
  rowwise() %>%
  mutate(
    traveltime = case_when(
      traveltime == 1 ~ runif(1, 1, 15),
      traveltime == 2 ~ runif(1, 15, 30),
      traveltime == 3 ~ runif(1, 30, 60),
      traveltime == 4 ~ runif(1, 60, 90)
    ),
    studytime = case_when(
      studytime == 1 ~ runif(1, 0, 2),
      studytime == 2 ~ runif(1, 2, 5),
      studytime == 3 ~ runif(1, 5, 10),
      studytime == 4 ~ runif(1, 10, 15)
    )
  )
# rename factors that might be hard to interpret
df_students <- df_students %>%
  rename(
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

