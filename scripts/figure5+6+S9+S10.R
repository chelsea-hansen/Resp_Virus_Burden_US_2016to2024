rm(list=ls())

library(tidyverse)
library(cowplot)
library(data.table)

'%notin%' = Negate("%in%")
source("R/summary_functions.R")
source("R/figure_functions.R")


ages <- c("<1", "1to4", "5to49", "50to64", "65+")
recorded_burden = readRDS("data/recorded_burden_public.rds") %>% mutate(age=factor(agegrp,levels=c("<1","1to4","5to49","50to64","65+")))

hosp_model = readRDS("main_results/hosp_resp_any.rds") 

mort_model = readRDS("main_results/mort_resp_any.rds") 

hospital_seasonal = seasonal_summary(hosp_model, "pop_enrolled") %>% filter(season!="2015-2016") %>% 
  mutate(age = factor(age,levels=ages),cause="Hospitalization")

mortality_seasonal = seasonal_summary(mort_model, "pop_total") %>% filter(season!="2015-2016") %>% 
  mutate(age = factor(age,levels=ages), cause="Mortality")

both_outcomes = rbind(hospital_seasonal, mortality_seasonal) %>% 
  mutate(age = factor(age,levels=c("<1","1to4","5to49","50to64","65+")))

fig5 = plot_ensemble_with_recorded_v2(both_outcomes, recorded_burden, "flu","Rate")
fig5
ggsave(plot=fig5,"figures/figure5.png",height=5,width=10,units="in")
ggsave(plot=fig5,"figures/TIFF/figure5.tiff",height=5,width=10,units="in")

fig6 = plot_ensemble_with_recorded_v2(both_outcomes, recorded_burden, "rsv","Rate")
fig6
ggsave(plot=fig6,"figures/figure6.png",height=5,width=10,units="in")
ggsave(plot=fig6,"figures/TIFF/figure6.tiff",height=5,width=10,units="in")

figS9 = plot_ensemble_with_recorded_v2(both_outcomes %>% filter(season %in% c("2022-2023","2023-2024","2024-2025")), recorded_burden%>% filter(season %in% c("2022-2023","2023-2024","2024-2025")), "cov","Rate")
figS9
ggsave(plot=figS9,"figures/figureS9.png",height=8.5,width=11,units="in")
ggsave(plot=figS9,"figures/TIFF/figureS9.tiff",height=8.5,width=11,units="in")

figS10 = plot_ensemble_with_recorded_v3(hospital_seasonal, recorded_burden, "hmpv",'HMPV Hosp.',"rhino",'RV Hosp.')
figS10
ggsave(plot=figS10,"figures/figureS10.png",height=8.5,width=11,units="in")
ggsave(plot=figS10,"figures/TIFF/figureS10.tiff",height=8.5,width=11,units="in")
