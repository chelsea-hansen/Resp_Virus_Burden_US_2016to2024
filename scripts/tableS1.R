rm(list=ls())
library(tidyverse)
library(readxl)
library(data.table)
library(writexl)
library(purrr)

dat = readRDS("data/data_public.rds") 
source("R/summary_functions.R")
source("R/table_functions.R")

seasons = c("2016-2017","2017-2018","2018-2019","2019-2020","2022-2023","2023-2024")

hosp_model1 = readRDS("model_output/hosp_resp_any.rds")
hosp_model2 = readRDS("model_output/hosp_resp_1st.rds")
hosp_model3 = readRDS("model_output/hosp_respcirc_1st.rds")

#resp - any 
hosp1_part1 = compare_pre_and_post(hosp_model1, "pop_enrolled") %>% 
  select(age,period,"resp_any_rate"=combo_rate, "resp_any"=combo_count) %>% mutate(virus="combo")

hosp1_part2 = average_table(hosp_model1, "pop_enrolled",seasons)%>% select(-combo_rate, -combo_count) %>% 
  pivot_longer(cols=c(flu_rate:covid_count),names_to="id",values_to="value") %>% 
  separate(id, into=c("virus","measure"), sep="_") %>% 
  pivot_wider(names_from = measure,values_from=value) %>% 
  rename("resp_any_rate"=rate,"resp_any"=count) %>% 
  mutate(period="average") %>% 
  bind_rows(hosp1_part1) 

#resp - first 
hosp2_part1 = compare_pre_and_post(hosp_model2, "pop_enrolled") %>% 
  select(age,period,"resp_first_rate"=combo_rate, "resp_first"=combo_count) %>% mutate(virus="combo")

hosp2_part2 = average_table(hosp_model2, "pop_enrolled",seasons)%>% select(-combo_rate, -combo_count) %>% 
  pivot_longer(cols=c(flu_rate:covid_count),names_to="id",values_to="value") %>% 
  separate(id, into=c("virus","measure"), sep="_") %>% 
  pivot_wider(names_from = measure,values_from=value) %>% 
  rename("resp_first_rate"=rate,"resp_first"=count) %>% 
  mutate(period="average") %>% 
  bind_rows(hosp2_part1) 

#resp-circulatory first 
hosp3_part1 = compare_pre_and_post(hosp_model3, "pop_enrolled") %>% 
  select(age,period,"rc_first_rate"=combo_rate, "rc_first"=combo_count) %>% mutate(virus="combo")

hosp3_part2 = average_table(hosp_model3, "pop_enrolled",seasons)%>% select(-combo_rate, -combo_count) %>% 
  pivot_longer(cols=c(flu_rate:covid_count),names_to="id",values_to="value") %>% 
  separate(id, into=c("virus","measure"), sep="_") %>% 
  pivot_wider(names_from = measure,values_from=value) %>% 
  rename("rc_first_rate"=rate,"rc_first"=count) %>% 
  mutate(period="average") %>% 
  bind_rows(hosp3_part1) 

join_vars = c("age", "virus", "period")
hosp_tables <- list(
  hosp1_part2,
  hosp2_part2,
  hosp3_part2
) %>%
  reduce(left_join, by = join_vars)%>%
  select(all_of(join_vars), everything()) %>% 
  mutate(virus = factor(virus, levels=c("flu","rsv","hmpv","rhino","covid","combo")),
         age = factor(age,levels=c("<1","1to4","5to49","50to64","65+","All"))) %>% 
  arrange(period,virus,age) 

write_xlsx(hosp_tables,"tables/table_S1.xlsx")


