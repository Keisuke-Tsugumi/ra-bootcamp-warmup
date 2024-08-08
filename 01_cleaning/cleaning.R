
library(tidyverse)
library(readxl)

# (a) Semester Data -------------------------------------------------------

## load data
df_semester_dummy1 <- 
  read.csv("data/raw/semester_dummy/semester_data_1.csv") %>% 
  rename_with(~ as.character(df_semester_dummy1[1, ])) %>% 
  slice(-1)

df_semester_dummy2 <- 
  read.csv("data/raw/semester_dummy/semester_data_2.csv") %>% 
  rename_with(~ names(df_semester_dummy1))


## marge data
df_semester_dummy <- 
  df_semester_dummy1 %>% 
  mutate(across(-c(instnm), as.numeric)) %>% 
  bind_rows(., df_semester_dummy2) %>% 
  select(-Y) %>% 
  group_by(unitid) %>% 
  mutate(semester_start_year = if_else(cumsum(semester) == 1, year, NA)) %>%
  fill(semester_start_year, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(d_after_semester = if_else(year >= semester_start_year, 1, 0)) %>% 
  # データの開始時点からsemester制の場合には、semester_start_year, d_after_semesterをNAに変更
  mutate(semester_start_year = if_else(semester_start_year == 1991, NA, semester_start_year),
         d_after_semester = if_else(semester_start_year == 1991, NA, d_after_semester))


# (b) Gradrate Date -------------------------------------------------------

path <- "data/raw/outcome"

files_path <- list.files(path, full.names = T)
files_name <- list.files(path) %>% 
  str_sub(., 1, 4) %>% 
  paste0("df_outcome_", .)

list_df_outcome <- 
  map(files_path, \(path){
    read_xlsx(path)
  }) %>% 
  setNames(files_name)

list_df_outcome <- 
  list_df_outcome %>% 
  map(., \(df){
    df <- 
      df %>% 
      mutate(across(everything(), as.numeric)) %>% 
      mutate(women_gradrate_4yr = women_gradrate_4yr * 0.01,
             tot_gradrate_4yr = tot4yrgrads / totcohortsize,
             men_gradrate_4yr = m_4yrgrads / m_cohortsize) %>% 
      mutate(across(c(women_gradrate_4yr, tot_gradrate_4yr, men_gradrate_4yr), ~
                    round(., 3)))
  })

df_outcome_1991_2010 <- 
  list_df_outcome[1:19] %>% 
  bind_rows()












































