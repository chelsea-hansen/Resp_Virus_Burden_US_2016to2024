rm(list=ls())

library(tidyverse)
library(cowplot)
library(scales)
library(data.table)

'%notin%' = Negate("%in%")
source("R/summary_functions.R")

dat = readRDS("data/recorded_burden_public.rds") %>% mutate(age=agegrp)

post_only = c("2022-2023","2023-2024")
# Hospital  ---------------------------------------------------------------

hosp_model = readRDS("model_output/hosp_resp_any.rds") 

true_pop = readRDS("data/data_public.rds") %>% group_by(agegrp, season) %>% summarize(true_pop=mean(pop))
hosp_seasonal = seasonal_summary(hosp_model,"pop_enrolled")

#All ages 
recorded_cov = dat %>% 
  left_join(true_pop,by=c("season","agegrp")) %>% 
  mutate(cov_any_count = round(cov_any_rate/100000*true_pop),
         cov_1_2_count = round(cov_1or2_rate/100000*true_pop)) %>% 
  group_by(season) %>% 
  summarize(cov_any_count = sum(cov_any_count,na.rm=TRUE),
            cov_1_2_count = sum(cov_1_2_count,na.rm=TRUE))


combined = hosp_seasonal %>% 
  filter(model=="Ensemble") %>%
  left_join(true_pop, by=c("age"="agegrp","season")) %>% 
  mutate(combo_count = combo_median/100000*true_pop,
         flu_count = flu_median/100000*true_pop,
         rsv_count = rsv_median/100000*true_pop,
         hmpv_count = hmpv_median/100000*true_pop,
         rhino_count = rhino_median/100000*true_pop,
         cov_count = cov_median/100000*true_pop) %>% 
  group_by(season) %>% 
  summarize(combo_median = sum(combo_count),
            flu_median = sum(flu_count),
            rsv_median = sum(rsv_count),
            hmpv_median = sum(hmpv_count),
            rhino_median = sum(rhino_count),
            cov_median = sum(cov_count)) %>% 
  mutate(combo_median2 = combo_median - cov_median) %>% 
  left_join(recorded_cov, by="season") %>% 
  mutate(use_this_covid = case_when(season %in% post_only ~cov_median,
                                    season %notin% post_only ~cov_any_count),
         plot_total = use_this_covid + combo_median2)

combined_long = combined %>% 
  pivot_longer(cols=c("flu_median","rsv_median","hmpv_median","rhino_median","use_this_covid"),
               names_to = "virus",values_to="burden") %>% 
  mutate(virus = factor(virus, levels=c("use_this_covid","flu_median","rsv_median","hmpv_median","rhino_median"),
                        labels=c("COVID-19","Influenza",'RSV',"HMPV","RV")),
         burden = ifelse(burden<0, 0, burden),
         age="All Ages")

#Over 65
recorded_cov_65 = dat %>% 
  filter(age=="65+") %>% 
  left_join(true_pop,by=c("season","agegrp")) %>% 
  mutate(cov_any_count = round(cov_any_rate/100000*true_pop),
         cov_1_2_count = round(cov_1or2_rate/100000*true_pop)) %>% 
  group_by(season) %>% 
  summarize(cov_any_count = sum(cov_any_count,na.rm=TRUE),
            cov_1_2_count = sum(cov_1_2_count,na.rm=TRUE))


combined_65 = hosp_seasonal %>% 
  filter(model=="Ensemble", age=="65+") %>%
  left_join(true_pop, by=c("age"="agegrp","season")) %>% 
  mutate(combo_count = combo_median/100000*true_pop,
         flu_count = flu_median/100000*true_pop,
         rsv_count = rsv_median/100000*true_pop,
         hmpv_count = hmpv_median/100000*true_pop,
         rhino_count = rhino_median/100000*true_pop,
         cov_count = cov_median/100000*true_pop) %>% 
  group_by(season) %>% 
  summarize(combo_median = sum(combo_count),
            flu_median = sum(flu_count),
            rsv_median = sum(rsv_count),
            hmpv_median = sum(hmpv_count),
            rhino_median = sum(rhino_count),
            cov_median = sum(cov_count)) %>% 
  mutate(combo_median2 = combo_median - cov_median) %>% 
  left_join(recorded_cov_65, by="season") %>% 
  mutate(use_this_covid = case_when(season %in% post_only ~cov_median,
                                    season %notin% post_only ~cov_any_count),
         plot_total = use_this_covid + combo_median2)

combined_long_65 = combined_65 %>% 
  pivot_longer(cols=c("flu_median","rsv_median","hmpv_median","rhino_median","use_this_covid"),
               names_to = "virus",values_to="burden") %>% 
  mutate(virus = factor(virus, levels=c("use_this_covid","flu_median","rsv_median","hmpv_median","rhino_median"),
                        labels=c("COVID-19","Influenza",'RSV',"HMPV","RV")),
         burden = ifelse(burden<0, 0, burden),
         age="65+ years")

# <5 years 
recorded_cov_less5 = dat %>% 
  filter(age %in% c("<1","1to4")) %>% 
  left_join(true_pop,by=c("season","agegrp")) %>% 
  mutate(cov_any_count = round(cov_any_rate/100000*true_pop),
         cov_1_2_count = round(cov_1or2_rate/100000*true_pop)) %>% 
  group_by(season) %>% 
  summarize(cov_any_count = sum(cov_any_count,na.rm=TRUE),
            cov_1_2_count = sum(cov_1_2_count,na.rm=TRUE))


combined_less5 = hosp_seasonal %>% 
  filter(model=="Ensemble",age %in% c("<1","1to4") ) %>%
  left_join(true_pop, by=c("age"="agegrp","season")) %>% 
  mutate(combo_count = combo_median/100000*true_pop,
         flu_count = flu_median/100000*true_pop,
         rsv_count = rsv_median/100000*true_pop,
         hmpv_count = hmpv_median/100000*true_pop,
         rhino_count = rhino_median/100000*true_pop,
         cov_count = cov_median/100000*true_pop) %>% 
  group_by(season) %>% 
  summarize(combo_median = sum(combo_count),
            flu_median = sum(flu_count),
            rsv_median = sum(rsv_count),
            hmpv_median = sum(hmpv_count),
            rhino_median = sum(rhino_count),
            cov_median = sum(cov_count)) %>% 
  mutate(combo_median2 = combo_median - cov_median) %>% 
  left_join(recorded_cov_less5, by="season") %>% 
  mutate(use_this_covid = case_when(season %in% post_only ~cov_median,
                                    season %notin% post_only ~cov_any_count),
         plot_total = use_this_covid + combo_median2)

combined_long_less5 = combined_less5 %>% 
  pivot_longer(cols=c("flu_median","rsv_median","hmpv_median","rhino_median","use_this_covid"),
               names_to = "virus",values_to="burden") %>% 
  mutate(virus = factor(virus, levels=c("use_this_covid","flu_median","rsv_median","hmpv_median","rhino_median"),
                        labels=c("COVID-19","Influenza",'RSV',"HMPV","RV")),
         burden = ifelse(burden<0, 0, burden),
         age="<5 years")

combined_hosp_all = rbind(combined_long, combined_long_65, combined_long_less5) %>% 
  mutate(outcome="Hospitalizations")



# Mortality ---------------------------------------------------------------
mort_model = readRDS("model_output/mort_resp_any.rds") 
mort_seasonal = seasonal_summary(mort_model, "pop_total")


#All ages
recorded_cov_mort = dat %>% 
  left_join(true_pop,by=c("season","agegrp")) %>% 
  mutate(cov_mc_count = cov_mc_rate/100000*true_pop,
         cov_uc_count = cov_uc_rate/100000*true_pop) %>% 
  group_by(season) %>% 
  summarize(cov_mc_count = sum(cov_mc_count,na.rm=TRUE),
            cov_uc_count = sum(cov_uc_count,na.rm=TRUE))

combined_mort = mort_seasonal %>% 
  filter(model=="Ensemble") %>%
  mutate(combo_count = combo_median/100000*pop,
         flu_count = flu_median/100000*pop,
         rsv_count = rsv_median/100000*pop,
         hmpv_count = hmpv_median/100000*pop,
         rhino_count = rhino_median/100000*pop,
         cov_count = cov_median/100000*pop) %>% 
  group_by(season) %>% 
  summarize(combo_median = sum(combo_count),
            flu_median = sum(flu_count),
            rsv_median = sum(rsv_count),
            hmpv_median = sum(hmpv_count),
            rhino_median = sum(rhino_count),
            cov_median = sum(cov_count)) %>% 
  mutate(combo_median2 = combo_median - cov_median) %>% 
  left_join(recorded_cov_mort, by="season") %>% 
  mutate(use_this_covid = case_when(season %in% post_only ~cov_median,
                                    season %notin% post_only ~cov_mc_count),
         plot_total = use_this_covid + combo_median2)

combined_long_mort = combined_mort %>% 
  pivot_longer(cols=c("flu_median","rsv_median","hmpv_median","rhino_median","use_this_covid"),
               names_to = "virus",values_to="burden") %>% 
  mutate(virus = factor(virus, levels=c("use_this_covid","flu_median","rsv_median","hmpv_median","rhino_median"),
                        labels=c("COVID-19","Influenza",'RSV',"HMPV","RV")),
         burden = ifelse(burden<0, 0, burden),
         age="All Ages")

# 65+ versions 

recorded_cov_mort_65 = dat %>% 
  filter(age=="65+") %>% 
  left_join(true_pop,by=c("season","agegrp")) %>% 
  mutate(cov_mc_count = cov_mc_rate/100000*true_pop,
         cov_uc_count = cov_uc_rate/100000*true_pop) %>% 
  group_by(season) %>% 
  summarize(cov_mc_count = sum(cov_mc_count,na.rm=TRUE),
            cov_uc_count = sum(cov_uc_count,na.rm=TRUE))

combined_mort_65 = mort_seasonal %>% 
  filter(model=="Ensemble",age=="65+") %>%
  mutate(combo_count = combo_median/100000*pop,
         flu_count = flu_median/100000*pop,
         rsv_count = rsv_median/100000*pop,
         hmpv_count = hmpv_median/100000*pop,
         rhino_count = rhino_median/100000*pop,
         cov_count = cov_median/100000*pop) %>% 
  group_by(season) %>% 
  summarize(combo_median = sum(combo_count),
            flu_median = sum(flu_count),
            rsv_median = sum(rsv_count),
            hmpv_median = sum(hmpv_count),
            rhino_median = sum(rhino_count),
            cov_median = sum(cov_count)) %>% 
  mutate(combo_median2 = combo_median - cov_median) %>% 
  left_join(recorded_cov_mort_65, by="season") %>% 
  mutate(use_this_covid = case_when(season %in% post_only ~cov_median,
                                    season %notin% post_only ~cov_mc_count),
         plot_total = use_this_covid + combo_median2)

combined_long_mort_65 = combined_mort_65 %>% 
  pivot_longer(cols=c("flu_median","rsv_median","hmpv_median","rhino_median","use_this_covid"),
               names_to = "virus",values_to="burden") %>% 
  mutate(virus = factor(virus, levels=c("use_this_covid","flu_median","rsv_median","hmpv_median","rhino_median"),
                        labels=c("COVID-19","Influenza",'RSV',"HMPV","RV")),
         burden = ifelse(burden<0, 0, burden),
         age="65+ years")


# under 5 versions 
recorded_cov_mort_less5 = dat %>% 
  filter(age %in% c("<1","1to4")) %>% 
  left_join(true_pop,by=c("season","agegrp")) %>% 
  mutate(cov_mc_count = cov_mc_rate/100000*true_pop,
         cov_uc_count = cov_uc_rate/100000*true_pop) %>% 
  group_by(season) %>% 
  summarize(cov_mc_count = sum(cov_mc_count,na.rm=TRUE),
            cov_uc_count = sum(cov_uc_count,na.rm=TRUE))

combined_mort_less5 = mort_seasonal %>% 
  filter(model=="Ensemble",age %in% c("<1","1to4")) %>%
  mutate(combo_count = combo_median/100000*pop,
         flu_count = flu_median/100000*pop,
         rsv_count = rsv_median/100000*pop,
         hmpv_count = hmpv_median/100000*pop,
         rhino_count = rhino_median/100000*pop,
         cov_count = cov_median/100000*pop) %>% 
  group_by(season) %>% 
  summarize(combo_median = sum(combo_count),
            flu_median = sum(flu_count),
            rsv_median = sum(rsv_count),
            hmpv_median = sum(hmpv_count),
            rhino_median = sum(rhino_count),
            cov_median = sum(cov_count)) %>% 
  mutate(combo_median2 = combo_median - cov_median) %>% 
  left_join(recorded_cov_mort_less5, by="season") %>% 
  mutate(use_this_covid = case_when(season %in% post_only ~cov_median,
                                    season %notin% post_only ~cov_mc_count),
         plot_total = use_this_covid + combo_median2)

combined_long_mort_less5 = combined_mort_less5 %>% 
  pivot_longer(cols=c("flu_median","rsv_median","hmpv_median","rhino_median","use_this_covid"),
               names_to = "virus",values_to="burden") %>% 
  mutate(virus = factor(virus, levels=c("use_this_covid","flu_median","rsv_median","hmpv_median","rhino_median"),
                        labels=c("COVID-19","Influenza",'RSV',"HMPV","RV")),
         burden = ifelse(burden<0, 0, burden),
         age="<5 years")



combined_mort_all = rbind(combined_long_mort, combined_long_mort_65, combined_long_mort_less5) %>% 
  mutate(outcome = "Deaths")



combined_all = rbind(combined_hosp_all, 
                     combined_mort_all %>% rename("cov_any_count"=cov_mc_count,
                                                  "cov_1_2_count" = cov_uc_count)) %>% 
  mutate(outcome = factor(outcome,levels=c("Hospitalizations",'Deaths')),
         age=factor(age,levels=c("All Ages","65+ years","<5 years")))

fig3 = ggplot(combined_all %>% filter(season!="2015-2016"))+
  theme_bw()+
  geom_bar(aes(x=season, y=burden, fill=virus),stat="identity")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(name=NULL, values=c("darkorchid1","red","goldenrod2","steelblue","olivedrab"))+
  labs(x=NULL, y=NULL)+
  facet_wrap(~age+outcome,scale="free_y",ncol=2)+
  theme(axis.text.x = element_text(angle=90),
        legend.position="top")
fig3

ggsave(plot=fig3,"figures/figure3.png",height=8,width=6,units="in")
ggsave(plot=fig3,"figures/TIFF/figure3.tiff",height=8,width=6,units="in")
