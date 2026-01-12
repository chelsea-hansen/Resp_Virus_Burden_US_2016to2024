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

mort_model1 = readRDS("model_output/mort_resp_any.rds")
mort_model2 = readRDS("model_output/mort_resp_underlying.rds")
mort_model3 = readRDS("model_output/mort_respcirc_underlying.rds")

#resp - mc 
mort1_part1 = compare_pre_and_post(mort_model1, "pop_total") %>% 
  select(age,period,"resp_mc_rate"=combo_rate, "resp_mc"=combo_count) %>% mutate(virus="combo")

mort1_part2 = average_table(mort_model1, "pop_total",seasons)%>% select(-combo_rate, -combo_count) %>% 
  pivot_longer(cols=c(flu_rate:covid_count),names_to="id",values_to="value") %>% 
  separate(id, into=c("virus","measure"), sep="_") %>% 
  pivot_wider(names_from = measure,values_from=value) %>% 
  rename("resp_mc_rate"=rate,"resp_mc"=count) %>% 
  mutate(period="average") %>% 
  bind_rows(mort1_part1) 

#resp - uc 
mort2_part1 = compare_pre_and_post(mort_model2, "pop_total") %>% 
  select(age,period,"resp_uc_rate"=combo_rate, "resp_uc"=combo_count) %>% mutate(virus="combo")

mort2_part2 = average_table(mort_model2, "pop_total",seasons)%>% select(-combo_rate, -combo_count) %>% 
  pivot_longer(cols=c(flu_rate:covid_count),names_to="id",values_to="value") %>% 
  separate(id, into=c("virus","measure"), sep="_") %>% 
  pivot_wider(names_from = measure,values_from=value) %>% 
  rename("resp_uc_rate"=rate,"resp_uc"=count) %>% 
  mutate(period="average") %>% 
  bind_rows(mort2_part1) 

#resp-circulatory uc 
mort3_part1 = compare_pre_and_post(mort_model3, "pop_total") %>% 
  select(age,period,"rc_uc_rate"=combo_rate, "rc_uc"=combo_count) %>% mutate(virus="combo")

mort3_part2 = average_table(mort_model3, "pop_total",seasons)%>% select(-combo_rate, -combo_count) %>% 
  pivot_longer(cols=c(flu_rate:covid_count),names_to="id",values_to="value") %>% 
  separate(id, into=c("virus","measure"), sep="_") %>% 
  pivot_wider(names_from = measure,values_from=value) %>% 
  rename("rc_uc_rate"=rate,"rc_uc"=count) %>% 
  mutate(period="average") %>% 
  bind_rows(mort3_part1) 

join_vars = c("age", "virus", "period")
mort_tables <- list(
  mort1_part2,
  mort2_part2,
  mort3_part2
) %>%
  reduce(left_join, by = join_vars)%>%
  select(all_of(join_vars), everything()) %>% 
  mutate(virus = factor(virus, levels=c("flu","rsv","hmpv","rhino","covid","combo")),
         age = factor(age,levels=c("<1","1to4","5to49","50to64","65+","All"))) %>% 
  arrange(period,virus,age) 

write_xlsx(mort_tables,"tables/table_S2.xlsx")


