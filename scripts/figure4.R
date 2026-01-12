
rm(list=ls())

library(tidyverse)
library(cowplot)
library(data.table)

'%notin%' = Negate("%in%")
source("R/summary_functions.R")

dat = readRDS("data/data_public.rds") %>% mutate(age=agegrp)


pre_pandemic = c("2016-2017","2017-2018","2018-2019","2019-2020")
post_only = c("2022-2023","2023-2024")



# Hospital ----------------------------------------------------------------


hosp_model = readRDS("model_output/hosp_resp_any.rds") 

hosp_pre = average_across_seasons(hosp_model, "pop_enrolled",pre_pandemic) %>%
  mutate(period = "Pre-pandemic")

hosp_post = average_across_seasons(hosp_model, "pop_enrolled",post_only) %>% 
  mutate(period = "Post-pandemic")

hosp_all = rbind(hosp_pre, hosp_post) %>% 
  #filter(model=="Ensemble") %>% 
  pivot_longer(cols=c(flu_median:cov_upper), names_to="name",values_to="estimate")%>%
  separate(name, into = c("virus", "measure"), sep = "_") %>% 
  mutate(age = factor(age, levels=c("<1","1to4","5to49","50to64","65+")),
         virus = factor(virus, levels=c("flu", "rsv", "hmpv", "rhino","cov"),
                        labels=c("Influenza","RSV","HMPV","RV","COVID-19")))%>% 
  pivot_wider(names_from=measure, values_from=estimate) %>% 
  mutate(period = factor(period, levels=c("Pre-pandemic","Post-pandemic")),
         model = factor(model,levels=c("Model_A","Model_B","Model_C","Model_D","Model_E","Ensemble"),
                        labels=c("A","B","C","D","E","Ensemble")))


hosp_avg = ggplot(hosp_all %>% filter(model=="Ensemble",median>0))+
  theme_bw()+
  geom_errorbar(aes(x=virus, ymin=lower, ymax=upper,color=virus, group=period),linewidth=2,width=0,
                position=position_dodge(width=0.8))+
  geom_point(aes(x=virus,y=median,color=virus,shape=period),size=4,position=position_dodge(width=0.8),fill="white")+
  facet_wrap(~age,scales="free_y",ncol=5)+
  scale_color_manual(name=NULL, values=c("red","goldenrod2","steelblue","olivedrab","darkorchid"))+
  scale_shape_manual(name=NULL, values=c(21,24))+
  labs(x=NULL,y="Hospitalizations per 100,000")+
  theme(legend.position="top",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text = element_text(size=12),
        legend.text = element_text(size=15))
hosp_avg

# Mortality ---------------------------------------------------------------
mort_model = readRDS("model_output/mort_resp_any.rds") 

mort_pre = average_across_seasons(mort_model, "pop_total",pre_pandemic) %>%
  mutate(period = "Pre-pandemic")

mort_post = average_across_seasons(mort_model, "pop_total",post_only) %>% 
  mutate(period = "Post-pandemic")

mort_all = rbind(mort_pre, mort_post) %>% 
  # filter(model=="Ensemble") %>% 
  pivot_longer(cols=c(flu_median:cov_upper), names_to="name",values_to="estimate")%>%
  separate(name, into = c("virus", "measure"), sep = "_") %>% 
  mutate(age = factor(age, levels=c("<1","1to4","5to49","50to64","65+")),
         virus = factor(virus, levels=c("flu", "rsv", "hmpv", "rhino","cov"),
                        labels=c("Influenza","RSV","HMPV","RV","COVID-19")))%>% 
  pivot_wider(names_from=measure, values_from=estimate) %>% 
  mutate(period = factor(period, levels=c("Pre-pandemic","Post-pandemic")),
         model = factor(model,levels=c("Model_A","Model_B","Model_C","Model_D","Model_E","Ensemble"),
                        labels=c("A","B","C","D","E","Ensemble")))


mort_avg = ggplot(mort_all %>% filter(model=="Ensemble", median>0))+
  theme_bw()+
  geom_errorbar(aes(x=virus, ymin=lower, ymax=upper,color=virus, group=period),linewidth=2,width=0,
                position=position_dodge(width=0.8))+
  geom_point(aes(x=virus,y=median,color=virus,shape=period),size=4,position=position_dodge(width=0.8),fill="white")+
  facet_wrap(~age,scales="free_y",ncol=5)+
  scale_color_manual(name=NULL, values=c("red","goldenrod2","steelblue","olivedrab","darkorchid"))+
  scale_shape_manual(name=NULL, values=c(21,24))+
  labs(x=NULL,y="Deaths per 100,000")+
  theme(legend.position="none",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text = element_text(size=12),
        legend.text = element_text(size=15))
mort_avg
averages = plot_grid(hosp_avg, mort_avg, nrow=2, rel_heights = c(1,0.8),align="v")
averages
ggsave(plot=averages, "figures/figure4.png",height=6,width=12,units="in")
ggsave(plot=averages, "figures/TIFF/figure4.tiff",height=6,width=12,units="in")

