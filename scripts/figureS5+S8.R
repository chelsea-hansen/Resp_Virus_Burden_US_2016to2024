rm(list=ls())

source("R/summary_functions.r")
library(tidyverse)
library(data.table)
library(cowplot)

dat_hosp = readRDS("sensitivity_analysis/LOSO/LOSO_hospitalization.rds")
dat_mort = readRDS("sensitivity_analysis/LOSO/LOSO_mortality.rds")

seasons = c("2016-2017","2017-2018","2018-2019","2019-2020","2022-2023","2023-2024","2024-2025")
post_seasons =c("2022-2023","2023-2024","2024-2025")

hosp_avg = average_across_seasons_v2(dat_hosp, "pop_enrolled", seasons) %>% 
  mutate(age = factor(age,levels=c("<1","1to4","5to49","50to64","65+"))) %>% 
  select(-cov_median,-cov_lower,-cov_upper,-combo_median,-combo_lower,-combo_upper)

hosp_avg_cov = average_across_seasons_v2(dat_hosp, "pop_enrolled", post_seasons) %>% 
  mutate(age = factor(age,levels=c("<1","1to4","5to49","50to64","65+"))) %>% 
  select(model, age, version, cov_median,cov_lower,cov_upper)

hosp_avg = hosp_avg %>% 
  left_join(hosp_avg_cov, by=c("model","age","version")) %>% 
  mutate(version = factor(version,levels=c("all_years","drop_2016_17","drop_2017_18","drop_2018_19",
                                           "drop_2019_20","drop_2020_21","drop_2021_22",
                                           "drop_2022_23","drop_2023_24","drop_2024_25"),
                          labels=c("None","2016_17","2017_18","2018_19",
                                   "2019_20","2020_21","2021_22",
                                   "2022_23","2023_24","2024_25")))

mort_avg = average_across_seasons_v2(dat_mort, "pop_total", seasons) %>% 
  mutate(age = factor(age,levels=c("<1","1to4","5to49","50to64","65+")))%>% 
  select(-cov_median,-cov_lower,-cov_upper,-combo_median,-combo_lower,-combo_upper)

mort_avg_cov = average_across_seasons_v2(dat_mort, "pop_total", post_seasons) %>% 
  mutate(age = factor(age,levels=c("<1","1to4","5to49","50to64","65+"))) %>% 
  select(model, age, version, cov_median,cov_lower,cov_upper)

mort_avg = mort_avg %>% 
  left_join(mort_avg_cov, by=c("model","age","version"))%>% 
  mutate(version = factor(version,levels=c("all_years","drop_2016_17","drop_2017_18","drop_2018_19",
                                           "drop_2019_20","drop_2020_21","drop_2021_22",
                                           "drop_2022_23","drop_2023_24","drop_2024_25"),
                          labels=c("None","2016_17","2017_18","2018_19",
                                   "2019_20","2020_21","2021_22",
                                   "2022_23","2023_24","2024_25")))


# Hospitalization Plots ---------------------------------------------------
hosp_plot = hosp_avg %>% 
pivot_longer(cols = matches("_(median|lower|upper)$"),
names_to = c("virus", ".value"),
names_sep = "_") %>% 
  filter(virus!="combo") %>% 
  mutate(virus = factor(virus, levels=c("flu","rsv","hmpv","rhino","cov"),
                        labels=c("Influenza",'RSV',"HMPV","RV/EV","SARS-CoV-2")),
         age = factor(age, levels=c("<1","1to4","5to49","50to64","65+")))


FigS5=ggplot(hosp_plot %>% filter(model=="Ensemble"))+
  theme_bw()+
  geom_point(aes(x=version, y=median))+
  geom_errorbar(aes(x=version,ymin=lower, ymax=upper),width=0)+
  facet_wrap(~virus+age,scales="free_y")+
  theme(axis.text.x=element_text(angle=90))+
  labs(x=NULL,y="Hospitalizations per 100,000")
FigS5
ggsave(plot=FigS5,"figures/figureS5.png",height=8.5,width=11,units="in")
ggsave(plot=FigS5,"figures/TIFF/figureS5.tiff",height=8.5,width=11,units="in")


mort_plot = mort_avg %>% 
  pivot_longer(cols = matches("_(median|lower|upper)$"),
               names_to = c("virus", ".value"),
               names_sep = "_") %>% 
  filter(virus!="combo") %>% 
  mutate(virus = factor(virus, levels=c("flu","rsv","hmpv","rhino","cov"),
                        labels=c("Influenza",'RSV',"HMPV","RV/EV","SARS-CoV-2")),
         age = factor(age, levels=c("<1","1to4","5to49","50to64","65+")))


FigS8=ggplot(mort_plot %>% filter(model=="Ensemble"))+
  theme_bw()+
  geom_point(aes(x=version, y=median))+
  geom_errorbar(aes(x=version,ymin=lower, ymax=upper),width=0)+
  facet_wrap(~virus+age,scales="free_y")+
  theme(axis.text.x=element_text(angle=90))+
  labs(x=NULL,y="Deaths per 100,000")
FigS8
ggsave(plot=FigS8,"figures/figureS8.png",height=8.5,width=11,units="in")
ggsave(plot=FigS8,"figures/TIFF/figureS8.tiff",height=8.5,width=11,units="in")


