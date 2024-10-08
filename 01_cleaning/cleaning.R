
library(tidyverse)
library(readxl)
library(stringr)

# (a) Semester Data -------------------------------------------------------

## load data
df_semester_dummy1 <- 
  read.csv("data/raw/semester_dummy/semester_data_1.csv")

df_semester_dummy2 <- 
  read.csv("data/raw/semester_dummy/semester_data_2.csv")

## rename vars
df_semester_dummy1 <- 
  df_semester_dummy1 %>% 
  rename_with(~ as.character(df_semester_dummy1[1, ])) %>% 
  slice(-1)

df_semester_dummy2 <- 
  df_semester_dummy2 %>% 
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


# Covariates Data ---------------------------------------------------------

df_covariates <- 
  read_xlsx("data/raw/covariates/covariates.xlsx")

df_covariates <- 
  df_covariates %>% 
  rename(unitid = university_id) %>% 
  mutate(unitid = str_replace_all(unitid, "aaaa", ""),
         across(-c(category), as.numeric)) %>% 
  pivot_wider(names_from = category, values_from = value) %>% 
  filter(year >= 1991 & year <= 2010)

df_covariates <- 
  df_outcome_1991_2010 %>% 
  distinct(unitid, year) %>% 
  left_join(., df_covariates, by = c("unitid", "year")) %>% 
  arrange(unitid, year)


# (d) Master Data -------------------------------------------------------------

df_master_data <-
  df_semester_dummy %>% 
  left_join(., df_covariates, by = c("unitid", "year")) %>% 
  left_join(., df_outcome_1991_2010, by = c("unitid", "year"))

# save data ---------------------------------------------------------------

saveRDS(df_master_data, "data/analytic/df_master_data.rds")
