
library(tidyverse)
library(gt)
library(gtsummary)

# load data ---------------------------------------------------------------

df_master_data <- 
  read_rds("data/analytic/df_master_data.rds")


# (a) 記述統計 ----------------------------------------------------------------

# NAを数える
df_master_data %>% 
  summary()

df_master_data <- 
  df_master_data %>% 
  mutate(d_switcher = if_else(!is.na(semester_start_year), 1, 0),
         .after = quarter)

# table1
df_master_data %>% 
  tbl_summary(include = c(semester, tot_gradrate_4yr, women_gradrate_4yr,
                          men_gradrate_4yr, faculty, totcohortsize,
                          instatetuition, costs),
              by = d_switcher,
              statistic = list(all_continuous() ~ "{mean}({sd})"),
              missing = "no") %>% 
  add_overall()


# figre1
df_for_figre1 <- 
  df_master_data %>% 
  group_by(year) %>% 
  summarise(semester_rate = mean(semester),
            gradrate_4yr = mean(tot_gradrate_4yr)) %>% 
  ungroup()

df_for_figre1 %>% 
  ggplot(data = ., aes(x = year, y = semester_rate)) +
  geom_line() +
  theme_classic()

df_for_figre1 %>% 
  ggplot(data = ., aes(x = year, y = gradrate_4yr)) +
  geom_line() +
  theme_classic()

# scatter plot

df_master_data <- 
  df_master_data %>% 
  mutate(women_rate = w_cohortsize / totcohortsize,
         white_rate = white_cohortsize / totcohortsize)

xvars <- c("women_rate", "white_rate", "instatetuition")

map(xvars, \(xvar){
  ggplot(data = df_master_data, aes_string(x = xvar, y = "tot_gradrate_4yr")) +
  geom_point() +
  theme_classic()
})  


# regression

result <- 
  lm(tot_gradrate_4yr ~ d_after_semester, data = df_master_data)

modelsummary::modelsummary(result)
  
  
  
  
  
  
  
  









