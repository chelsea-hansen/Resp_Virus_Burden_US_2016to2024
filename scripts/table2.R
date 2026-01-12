rm(list=ls())
library(tidyverse)
library(readxl)
library(data.table)
library(writexl)

dat = readRDS("data/data_public.rds") 
source("R/summary_functions.R")
source("R/table_functions.R")

seasons = c("2016-2017","2017-2018","2018-2019","2019-2020","2022-2023","2023-2024")

hosp_model = readRDS("model_output/hosp_resp_any.rds")
mort_model = readRDS("model_output/mort_resp_any.rds")


hosp_table1_part1 = compare_pre_and_post(hosp_model, "pop_enrolled") %>% 
  select(age,period,"rate"=combo_rate, "count"=combo_count) %>% mutate(virus="combo")

hosp_table1_part2 = average_table(hosp_model, "pop_enrolled",seasons)%>% select(-combo_rate, -combo_count) %>% 
  pivot_longer(cols=c(flu_rate:covid_count),names_to="id",values_to="value") %>% 
  separate(id, into=c("virus","measure"), sep="_") %>% 
  pivot_wider(names_from = measure,values_from=value) %>% 
  mutate(period="average") %>% 
  bind_rows(hosp_table1_part1) %>% 
  rename("hosp_rate"=rate,"hosp_count"=count)



mort_table1_part1 = compare_pre_and_post(mort_model, "pop_total") %>% 
  select(age,period,"rate"=combo_rate, "count"=combo_count) %>% mutate(virus="combo")

mort_table1_part2 = average_table(mort_model, "pop_total",seasons)%>% select(-combo_rate, -combo_count) %>% 
  pivot_longer(cols=c(flu_rate:covid_count),names_to="id",values_to="value") %>% 
  separate(id, into=c("virus","measure"), sep="_") %>% 
  pivot_wider(names_from = measure,values_from=value) %>% 
  mutate(period="average") %>% 
  bind_rows(mort_table1_part1) %>% 
  rename("mort_rate"=rate,"mort_count"=count)

join_outcomes = hosp_table1_part2 %>% 
  left_join(mort_table1_part2, by=c("age","period","virus")) %>% 
  mutate(virus = factor(virus, levels=c("flu","rsv","hmpv","rhino","covid","combo")),
         age = factor(age,levels=c("<1","1to4","5to49","50to64","65+","All"))) %>% 
  arrange(period,virus,age) %>% 
  select(period, age,virus, hosp_rate,hosp_count,mort_rate,mort_count)

write_xlsx(join_outcomes, "tables/table_2.xlsx") 
