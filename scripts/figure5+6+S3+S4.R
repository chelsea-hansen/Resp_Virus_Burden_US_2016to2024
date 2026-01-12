rm(list=ls())

library(tidyverse)
library(cowplot)

'%notin%' = Negate("%in%")
source("R/summary_functions.R")
source("R/figure_functions.R")


ages <- c("<1", "1to4", "5to49", "50to64", "65+")
recorded_burden = readRDS("data/recorded_burden_public.rds") %>% mutate(age=agegrp)

hosp_model = readRDS("model_output/hosp_resp_any.rds") 

mort_model = readRDS("model_output/mort_resp_any.rds") 

hospital_seasonal = seasonal_summary(hosp_model, "pop_enrolled") %>% filter(season!="2015-2016") %>% 
  mutate(age = factor(age,levels=ages),cause="Hospitalization")

mortality_seasonal = seasonal_summary(mort_model, "pop_total") %>% filter(season!="2015-2016") %>% 
  mutate(age = factor(age,levels=ages), cause="Mortality")

both_outcomes = rbind(hospital_seasonal, mortality_seasonal)

fig5 = plot_ensemble_with_recorded_v2(both_outcomes, recorded_burden, "flu","Rate")
fig5
ggsave(plot=fig5,"figures/figure5.png",height=5,width=10,units="in")
ggsave(plot=fig5,"figures/TIFF/figure5.tiff",height=5,width=10,units="in")

fig6 = plot_ensemble_with_recorded_v2(both_outcomes, recorded_burden, "rsv","Rate")
fig6
ggsave(plot=fig6,"figures/figure6.png",height=5,width=10,units="in")
ggsave(plot=fig6,"figures/TIFF/figure6.tiff",height=5,width=10,units="in")

figS3 = plot_ensemble_with_recorded_v2(both_outcomes %>% filter(season %in% c("2022-2023","2023-2024")), recorded_burden%>% filter(season %in% c("2022-2023","2023-2024")), "cov","Rate")
figS3
ggsave(plot=figS3,"figures/figureS3.png",height=5,width=10,units="in")
ggsave(plot=figS3,"figures/TIFF/figureS3.tiff",height=5,width=10,units="in")

figS4 = plot_ensemble_with_recorded_v3(hospital_seasonal, recorded_burden, "hmpv",'HMPV Hosp.',"rhino",'RV Hosp.')
figS4
ggsave(plot=figS4,"figures/figureS4.png",height=5,width=10,units="in")
ggsave(plot=figS4,"figures/TIFF/figureS4.tiff",height=5,width=10,units="in")
