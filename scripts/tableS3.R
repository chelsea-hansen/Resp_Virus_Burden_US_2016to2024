rm(list=ls())
library(tidyverse)
library(data.table)
library(writexl)
'%notin%' = Negate('%in%')

dat = readRDS("data/data_public.rds") 
recorded_burden = readRDS("data/recorded_burden_public.rds") 

source("R/summary_functions.R")
ages = c("<1", "1to4", "5to49", "50to64", "65+")



# Hospitalizations --------------------------------------------------------


hosp_model = readRDS("model_output/hosp_resp_any.rds") %>% 
  filter(date>='2016-07-01')

hospital_seasonal = seasonal_summary(hosp_model, "pop_enrolled") %>% filter(season!="2015-2016") %>% 
  mutate(age = factor(age,levels=ages),
         cause = "Hospitalization")

compare_hosp = hospital_seasonal %>% filter(model=="Ensemble") %>% 
  left_join(recorded_burden,by=c("age"="agegrp","season"))

hosp_diffs = compare_hosp %>% 
  mutate(diff_flu = ((flu_median - flu_any_rate)/flu_any_rate),
         diff_rsv = ((rsv_median - rsv_any_rate)/rsv_any_rate),
         diff_hmpv = ((hmpv_median - hmpv_any_rate)/hmpv_any_rate),
         diff_rhino = ((rhino_median - rhino_any_rate)/rhino_any_rate),
         diff_cov = ((cov_median - cov_any_rate)/cov_any_rate))


hosp_diffs_pre = hosp_diffs %>% 
  filter(season %in% c("2016-2017","2017-2018","2018-2019","2019-2020")) %>% 
  group_by(age) %>% 
  summarize(flu_avg = mean(diff_flu),
            flu_se = sd(diff_flu)/sqrt(4),
            rsv_avg = mean(diff_rsv),
            rsv_se = sd(diff_rsv)/sqrt(4),
            hmpv_avg = mean(diff_hmpv),
            hmpv_se = sd(diff_hmpv)/sqrt(4),
            rhino_avg = mean(diff_rhino),
            rhino_se = sd(diff_rhino)/sqrt(4)) %>% 
  mutate(flu_lower = flu_avg - (1.96*flu_se),
         rsv_lower = rsv_avg - (1.96*rsv_se),
         hmpv_lower = hmpv_avg - (1.96*hmpv_se),
         rhino_lower = rhino_avg - (1.96*rhino_se),
         flu_upper = flu_avg + (1.96*flu_se),
         rsv_upper = rsv_avg + (1.96*rsv_se),
         hmpv_upper = hmpv_avg + (1.96*hmpv_se),
         rhino_upper = rhino_avg + (1.96*rhino_se),
         flu_avg = round(flu_avg*100,1),
         flu_lower = round(flu_lower*100,1),
         flu_upper = round(flu_upper*100,1),
         rsv_avg = round(rsv_avg*100,1),
         rsv_lower = round(rsv_lower*100,1),
         rsv_upper = round(rsv_upper*100,1),
         hmpv_avg = round(hmpv_avg*100,1),
         hmpv_lower = round(hmpv_lower*100,1),
         hmpv_upper = round(hmpv_upper*100,1),
         rhino_avg = round(rhino_avg*100,1),
         rhino_lower = round(rhino_lower*100,1),
         rhino_upper = round(rhino_upper*100,1),
         flu_diff = paste0(sprintf("%.1f", flu_avg),"\n(",sprintf("%.1f", flu_lower),"-",sprintf("%.1f", flu_upper),")"),
         rsv_diff = paste0(sprintf("%.1f", rsv_avg),"\n(",sprintf("%.1f", rsv_lower),"-",sprintf("%.1f", rsv_upper),")"),
         hmpv_diff = paste0(sprintf("%.1f", hmpv_avg),"\n(",sprintf("%.1f", hmpv_lower),"-",sprintf("%.1f", hmpv_upper),")"),
         rhino_diff = paste0(sprintf("%.1f", rhino_avg),"\n(",sprintf("%.1f", rhino_lower),"-",sprintf("%.1f", rhino_upper),")")) %>% 
  select(age, flu_diff,rsv_diff,hmpv_diff,rhino_diff) %>% 
  mutate(period = "pre-pandemic")



hosp_diffs_post = hosp_diffs %>% 
  filter(season %in% c("2022-2023","2023-2024")) %>% 
  group_by(age) %>% 
  summarize(flu_avg = mean(diff_flu),
            flu_se = sd(diff_flu)/sqrt(2),
            rsv_avg = mean(diff_rsv),
            rsv_se = sd(diff_rsv)/sqrt(2),
            hmpv_avg = mean(diff_hmpv),
            hmpv_se = sd(diff_hmpv)/sqrt(2),
            rhino_avg = mean(diff_rhino),
            rhino_se = sd(diff_rhino)/sqrt(2),
            cov_avg = mean(diff_cov),
            cov_se = sd(diff_cov)/sqrt(2),) %>% 
  mutate(flu_lower = flu_avg - (1.96*flu_se),
         rsv_lower = rsv_avg - (1.96*rsv_se),
         hmpv_lower = hmpv_avg - (1.96*hmpv_se),
         rhino_lower = rhino_avg - (1.96*rhino_se),
         cov_lower = cov_avg - (1.96*cov_se),
         flu_upper = flu_avg + (1.96*flu_se),
         rsv_upper = rsv_avg + (1.96*rsv_se),
         hmpv_upper = hmpv_avg + (1.96*hmpv_se),
         rhino_upper = rhino_avg + (1.96*rhino_se),
         cov_upper = cov_avg + (1.96*cov_se),
         flu_avg = round(flu_avg*100,1),
         flu_lower = round(flu_lower*100,1),
         flu_upper = round(flu_upper*100,1),
         rsv_avg = round(rsv_avg*100,1),
         rsv_lower = round(rsv_lower*100,1),
         rsv_upper = round(rsv_upper*100,1),
         hmpv_avg = round(hmpv_avg*100,1),
         hmpv_lower = round(hmpv_lower*100,1),
         hmpv_upper = round(hmpv_upper*100,1),
         rhino_avg = round(rhino_avg*100,1),
         rhino_lower = round(rhino_lower*100,1),
         rhino_upper = round(rhino_upper*100,1),
         cov_avg = round(cov_avg*100,1),
         cov_lower = round(cov_lower*100,1),
         cov_upper = round(cov_upper*100,1),
         flu_diff = paste0(sprintf("%.1f", flu_avg),"\n(",sprintf("%.1f", flu_lower),"-",sprintf("%.1f", flu_upper),")"),
         rsv_diff = paste0(sprintf("%.1f", rsv_avg),"\n(",sprintf("%.1f", rsv_lower),"-",sprintf("%.1f", rsv_upper),")"),
         hmpv_diff = paste0(sprintf("%.1f", hmpv_avg),"\n(",sprintf("%.1f", hmpv_lower),"-",sprintf("%.1f", hmpv_upper),")"),
         rhino_diff = paste0(sprintf("%.1f", rhino_avg),"\n(",sprintf("%.1f", rhino_lower),"-",sprintf("%.1f", rhino_upper),")"),
         cov_diff = paste0(sprintf("%.1f", cov_avg),"\n(",sprintf("%.1f", cov_lower),"-",sprintf("%.1f", cov_upper),")") ) %>% 
  select(age, flu_diff,rsv_diff,hmpv_diff,rhino_diff,cov_diff) %>% 
  mutate(period="post_pandemic")


hosp_diffs_all = hosp_diffs %>% 
  filter(season %in% c("2016-2017","2017-2018","2018-2019","2019-2020","2022-2023","2023-2024")) %>% 
  group_by(age) %>% 
  summarize(flu_avg = mean(diff_flu),
            flu_se = sd(diff_flu)/sqrt(6),
            rsv_avg = mean(diff_rsv),
            rsv_se = sd(diff_rsv)/sqrt(6),
            hmpv_avg = mean(diff_hmpv),
            hmpv_se = sd(diff_hmpv)/sqrt(6),
            rhino_avg = mean(diff_rhino),
            rhino_se = sd(diff_rhino)/sqrt(6)) %>% 
  mutate(flu_lower = flu_avg - (1.96*flu_se),
         rsv_lower = rsv_avg - (1.96*rsv_se),
         hmpv_lower = hmpv_avg - (1.96*hmpv_se),
         rhino_lower = rhino_avg - (1.96*rhino_se),
         flu_upper = flu_avg + (1.96*flu_se),
         rsv_upper = rsv_avg + (1.96*rsv_se),
         hmpv_upper = hmpv_avg + (1.96*hmpv_se),
         rhino_upper = rhino_avg + (1.96*rhino_se),
         flu_avg = round(flu_avg*100,1),
         flu_lower = round(flu_lower*100,1),
         flu_upper = round(flu_upper*100,1),
         rsv_avg = round(rsv_avg*100,1),
         rsv_lower = round(rsv_lower*100,1),
         rsv_upper = round(rsv_upper*100,1),
         hmpv_avg = round(hmpv_avg*100,1),
         hmpv_lower = round(hmpv_lower*100,1),
         hmpv_upper = round(hmpv_upper*100,1),
         rhino_avg = round(rhino_avg*100,1),
         rhino_lower = round(rhino_lower*100,1),
         rhino_upper = round(rhino_upper*100,1),
         flu_diff = paste0(sprintf("%.1f", flu_avg),"\n(",sprintf("%.1f", flu_lower),"-",sprintf("%.1f", flu_upper),")"),
         rsv_diff = paste0(sprintf("%.1f", rsv_avg),"\n(",sprintf("%.1f", rsv_lower),"-",sprintf("%.1f", rsv_upper),")"),
         hmpv_diff = paste0(sprintf("%.1f", hmpv_avg),"\n(",sprintf("%.1f", hmpv_lower),"-",sprintf("%.1f", hmpv_upper),")"),
         rhino_diff = paste0(sprintf("%.1f", rhino_avg),"\n(",sprintf("%.1f", rhino_lower),"-",sprintf("%.1f", rhino_upper),")")) %>% 
  select(age, flu_diff,rsv_diff,hmpv_diff,rhino_diff) %>% 
  mutate(period="All")


hosp_diffs_table = bind_rows(hosp_diffs_pre,hosp_diffs_post,hosp_diffs_all) %>% 
  pivot_longer(cols=c(flu_diff,rsv_diff,hmpv_diff,rhino_diff,cov_diff),names_to="virus",values_to="diff") %>% 
  pivot_wider(id_cols=c(age,virus),
              names_from = period,
              values_from = diff) %>% 
  mutate(virus = factor(virus,levels=c("flu_diff","rsv_diff","hmpv_diff","rhino_diff","cov_diff")),
         age =factor(age,levels=ages)) %>% 
  arrange(virus,age)


# Mortality ---------------------------------------------------------------


mort_model = readRDS("model_output/mort_resp_any.rds") %>% 
  filter(date>='2016-07-01')

mortality_seasonal = seasonal_summary(mort_model, "pop_total") %>% filter(season!="2015-2016") %>% 
  mutate(age = factor(age,levels=ages),
         cause = "mortality")

compare_mort = mortality_seasonal %>% filter(model=="Ensemble") %>% 
  left_join(recorded_burden, by=c("age"="agegrp","season"))

mort_diffs = compare_mort %>% 
  mutate(diff_flu = ((flu_median - flu_mc_rate)/flu_mc_rate),
         diff_rsv = ((rsv_median - rsv_mc_rate)/rsv_mc_rate),
         diff_cov = ((cov_median - cov_mc_rate)/cov_mc_rate))


mort_diffs_pre = mort_diffs %>% 
  filter(season %in% c("2016-2017","2017-2018","2018-2019","2019-2020")) %>% 
  group_by(age) %>% 
  summarize(flu_avg = mean(diff_flu),
            flu_se = sd(diff_flu)/sqrt(4),
            rsv_avg = mean(diff_rsv),
            rsv_se = sd(diff_rsv)/sqrt(4)) %>% 
  mutate(flu_lower = flu_avg - (1.96*flu_se),
         rsv_lower = rsv_avg - (1.96*rsv_se),
         flu_upper = flu_avg + (1.96*flu_se),
         rsv_upper = rsv_avg + (1.96*rsv_se),
         flu_avg = round(flu_avg*100,1),
         flu_lower = round(flu_lower*100,1),
         flu_upper = round(flu_upper*100,1),
         rsv_avg = round(rsv_avg*100,1),
         rsv_lower = round(rsv_lower*100,1),
         rsv_upper = round(rsv_upper*100,1),
         flu_diff = paste0(sprintf("%.1f", flu_avg),"\n(",sprintf("%.1f", flu_lower),"-",sprintf("%.1f", flu_upper),")"),
         rsv_diff = paste0(sprintf("%.1f", rsv_avg),"\n(",sprintf("%.1f", rsv_lower),"-",sprintf("%.1f", rsv_upper),")")) %>% 
  select(age, flu_diff,rsv_diff) %>% 
  mutate(period = "pre-pandemic")



mort_diffs_post = mort_diffs %>% 
  filter(season %in% c("2022-2023","2023-2024")) %>% 
  group_by(age) %>% 
  summarize(flu_avg = mean(diff_flu),
            flu_se = sd(diff_flu)/sqrt(2),
            rsv_avg = mean(diff_rsv),
            rsv_se = sd(diff_rsv)/sqrt(2),
            cov_avg = mean(diff_cov),
            cov_se = sd(diff_cov)/sqrt(2),) %>% 
  mutate(flu_lower = flu_avg - (1.96*flu_se),
         rsv_lower = rsv_avg - (1.96*rsv_se),
         cov_lower = cov_avg - (1.96*cov_se),
         flu_upper = flu_avg + (1.96*flu_se),
         rsv_upper = rsv_avg + (1.96*rsv_se),
         cov_upper = cov_avg + (1.96*cov_se),
         flu_avg = round(flu_avg*100,1),
         flu_lower = round(flu_lower*100,1),
         flu_upper = round(flu_upper*100,1),
         rsv_avg = round(rsv_avg*100,1),
         rsv_lower = round(rsv_lower*100,1),
         rsv_upper = round(rsv_upper*100,1),
         cov_avg = round(cov_avg*100,1),
         cov_lower = round(cov_lower*100,1),
         cov_upper = round(cov_upper*100,1),
         flu_diff = paste0(sprintf("%.1f", flu_avg),"\n(",sprintf("%.1f", flu_lower),"-",sprintf("%.1f", flu_upper),")"),
         rsv_diff = paste0(sprintf("%.1f", rsv_avg),"\n(",sprintf("%.1f", rsv_lower),"-",sprintf("%.1f", rsv_upper),")"),
         cov_diff = paste0(sprintf("%.1f", cov_avg),"\n(",sprintf("%.1f", cov_lower),"-",sprintf("%.1f", cov_upper),")") ) %>% 
  select(age, flu_diff,rsv_diff,cov_diff) %>% 
  mutate(period="post_pandemic")


mort_diffs_all = mort_diffs %>% 
  filter(season %in% c("2016-2017","2017-2018","2018-2019","2019-2020","2022-2023","2023-2024")) %>% 
  group_by(age) %>% 
  summarize(flu_avg = mean(diff_flu),
            flu_se = sd(diff_flu)/sqrt(6),
            rsv_avg = mean(diff_rsv),
            rsv_se = sd(diff_rsv)/sqrt(6)) %>% 
  mutate(flu_lower = flu_avg - (1.96*flu_se),
         rsv_lower = rsv_avg - (1.96*rsv_se),
         flu_upper = flu_avg + (1.96*flu_se),
         rsv_upper = rsv_avg + (1.96*rsv_se),
         flu_avg = round(flu_avg*100,1),
         flu_lower = round(flu_lower*100,1),
         flu_upper = round(flu_upper*100,1),
         rsv_avg = round(rsv_avg*100,1),
         rsv_lower = round(rsv_lower*100,1),
         rsv_upper = round(rsv_upper*100,1),
         flu_diff = paste0(sprintf("%.1f", flu_avg),"\n(",sprintf("%.1f", flu_lower),"-",sprintf("%.1f", flu_upper),")"),
         rsv_diff = paste0(sprintf("%.1f", rsv_avg),"\n(",sprintf("%.1f", rsv_lower),"-",sprintf("%.1f", rsv_upper),")")) %>% 
  select(age, flu_diff,rsv_diff) %>% 
  mutate(period="All")


mort_diffs_table = bind_rows(mort_diffs_pre,mort_diffs_post,mort_diffs_all) %>% 
  pivot_longer(cols=c(flu_diff,rsv_diff,cov_diff),names_to="virus",values_to="diff") %>% 
  pivot_wider(id_cols=c(age,virus),
              names_from = period,
              values_from = diff) %>% 
  mutate(virus = factor(virus,levels=c("flu_diff","rsv_diff","cov_diff")),
         age =factor(age,levels=ages)) %>% 
  arrange(virus,age)

tableS3 = hosp_diffs_table %>% 
  left_join(mort_diffs_table, by=c("age","virus"))


write_xlsx(tableS3,"tables/table_S3.xlsx")
