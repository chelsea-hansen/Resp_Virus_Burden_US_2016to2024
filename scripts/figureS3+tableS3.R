rm(list=ls())

source("R/summary_functions.r")
library(tidyverse)
library(data.table)
library(writexl)


dat = readRDS("sensitivity_analysis/LOVO/LOVO_hospitalization.rds")
base_version = readRDS("sensitivity_analysis/alternative_covariates/alternative_reference_versions.rds") %>% 
  filter(outcome=="hospitalization")
dat = rbind(dat, base_version)

seasons = c("2016-2017","2017-2018","2018-2019","2019-2020","2022-2023","2023-2024","2024-2025")
post_seasons =c("2022-2023","2023-2024","2024-2025")


hosp_LOO_covid = average_across_seasons(dat %>% filter(version=="LOO_covid"),"pop_enrolled",seasons) %>% 
  mutate(version="Leave out:SARS-CoV-2", shape_version = "Leave-One-Virus-Out") %>% filter(model=="Ensemble") 

hosp_LOO_rsv = average_across_seasons(dat %>% filter(version=="LOO_rsv"),"pop_enrolled",seasons) %>% 
  mutate(version="Leave out:RSV", shape_version = "Leave-One-Virus-Out") %>% filter(model=="Ensemble") 

hosp_LOO_hmpv = average_across_seasons(dat %>% filter(version=="LOO_hmpv"),"pop_enrolled",seasons) %>% 
  mutate(version="Leave out:HMPV", shape_version = "Leave-One-Virus-Out") %>% filter(model=="Ensemble") 

hosp_LOO_rhino = average_across_seasons(dat %>% filter(version=="LOO_rhino"),"pop_enrolled",seasons) %>% 
  mutate(version="Leave out:RV/EV", shape_version = "Leave-One-Virus-Out") %>% filter(model=="Ensemble") 

hosp_LOO_flu = average_across_seasons(dat %>% filter(version=="LOO_flu"),"pop_enrolled",seasons) %>% 
  mutate(version="Leave out:Flu", shape_version = "Leave-One-Virus-Out") %>% filter(model=="Ensemble") 

hosp_avg = average_across_seasons(dat %>% filter(version=="reference"),"pop_enrolled",seasons) %>% 
  mutate(version="Reference", shape_version = "Leave-One-Virus-Out") %>% filter(model=="Ensemble") 

hosp_avg1 = hosp_avg %>% 
  mutate(version="Leave out:SARS-CoV-2", shape_version = "Reference") 

hosp_avg2 = hosp_avg %>% 
  mutate(version="Leave out:RSV", shape_version = "Reference") 

hosp_avg3 = hosp_avg %>% 
  mutate(version="Leave out:HMPV", shape_version = "Reference") 

hosp_avg4 = hosp_avg %>% 
  mutate(version="Leave out:RV/EV", shape_version = "Reference") 

hosp_avg5 = hosp_avg %>% 
  mutate(version="Leave out:Flu", shape_version = "Reference") 

full_seasons = rbind(hosp_avg1,hosp_avg2,hosp_avg3,hosp_avg4, hosp_avg5, 
                  hosp_LOO_flu, hosp_LOO_covid, hosp_LOO_rsv,hosp_LOO_hmpv,hosp_LOO_rhino) %>% 
  pivot_longer(
    cols = matches("_(median|lower|upper)$"),
    names_to = c("virus", ".value"),
    names_sep = "_") %>% 
  filter(virus!="combo") %>% 
  mutate(virus = factor(virus, levels=c("flu","rsv","hmpv","rhino","cov"),
                        labels=c("Influenza",'RSV',"HMPV","RV/EV","SARS-CoV-2")),
         age = factor(age, levels=c("<1","1to4","5to49","50to64","65+")),
         version = factor(version,levels=c("Leave out:Flu","Leave out:RSV","Leave out:HMPV","Leave out:RV/EV","Leave out:SARS-CoV-2")),
         shape_version = factor(shape_version,levels=c("Reference","Leave-One-Virus-Out"))) %>% 
  filter(virus!="SARS-CoV-2")



hosp_LOO_covid_post = average_across_seasons(dat%>% filter(version=="LOO_covid"),"pop_enrolled",post_seasons) %>% 
  mutate(version="Leave out:SARS-CoV-2", shape_version = "Leave-One-Virus-Out") %>% filter(model=="Ensemble") 

hosp_LOO_rsv_post = average_across_seasons(dat %>% filter(version=="LOO_rsv"),"pop_enrolled",post_seasons) %>% 
  mutate(version="Leave out:RSV", shape_version = "Leave-One-Virus-Out") %>% filter(model=="Ensemble") 

hosp_LOO_hmpv_post = average_across_seasons(dat %>% filter(version=="LOO_hmpv"),"pop_enrolled",post_seasons) %>% 
  mutate(version="Leave out:HMPV", shape_version = "Leave-One-Virus-Out") %>% filter(model=="Ensemble") 

hosp_LOO_rhino_post = average_across_seasons(dat %>% filter(version=="LOO_rhino"),"pop_enrolled",post_seasons) %>% 
  mutate(version="Leave out:RV/EV", shape_version = "Leave-One-Virus-Out") %>% filter(model=="Ensemble") 

hosp_LOO_flu_post = average_across_seasons(dat %>% filter(version=="LOO_flu"),"pop_enrolled",post_seasons) %>% 
  mutate(version="Leave out:Flu", shape_version = "Leave-One-Virus-Out") %>% filter(model=="Ensemble") 

hosp_avg_post = average_across_seasons(dat %>% filter(version=="reference"),"pop_enrolled",post_seasons) %>% 
  mutate(version="Reference", shape_version = "Leave-One-Virus-Out") %>% filter(model=="Ensemble") 


hosp_avg1_post = hosp_avg_post %>% 
  mutate(version="Leave out:SARS-CoV-2", shape_version = "Reference") 

hosp_avg2_post = hosp_avg_post %>% 
  mutate(version="Leave out:RSV", shape_version = "Reference") 

hosp_avg3_post = hosp_avg_post %>% 
  mutate(version="Leave out:HMPV", shape_version = "Reference") 

hosp_avg4_post = hosp_avg_post %>% 
  mutate(version="Leave out:RV/EV", shape_version = "Reference") 

hosp_avg5_post = hosp_avg_post %>% 
  mutate(version="Leave out:Flu", shape_version = "Reference") 

post_seasons = rbind(hosp_avg1_post,hosp_avg2_post,hosp_avg3_post,hosp_avg4_post, hosp_avg5_post, 
                     hosp_LOO_flu_post, hosp_LOO_covid_post, hosp_LOO_rsv_post,hosp_LOO_hmpv_post,hosp_LOO_rhino_post) %>% 
  pivot_longer(
    cols = matches("_(median|lower|upper)$"),
    names_to = c("virus", ".value"),
    names_sep = "_") %>% 
  filter(virus!="combo") %>% 
  mutate(virus = factor(virus, levels=c("flu","rsv","hmpv","rhino","cov"),
                        labels=c("Influenza",'RSV',"HMPV","RV/EV","SARS-CoV-2")),
         age = factor(age, levels=c("<1","1to4","5to49","50to64","65+")),
         version = factor(version,levels=c("Leave out:Flu","Leave out:RSV","Leave out:HMPV","Leave out:RV/EV","Leave out:SARS-CoV-2")),
         shape_version = factor(shape_version,levels=c("Reference","Leave-One-Virus-Out"))) %>% 
  filter(virus=="SARS-CoV-2")


plot_dat = rbind(full_seasons, post_seasons)

LOO_hosp=ggplot(plot_dat %>% filter(median!=0)) +
  theme_bw()+
  geom_point(aes(x = virus,y = median,color = virus,shape = shape_version,group = shape_version),
             position = position_dodge(width = 0.6),size = 2) +
  geom_errorbar(aes(x = virus,ymin = lower,ymax = upper,color = virus,group = shape_version),position = position_dodge(width = 0.6),
                linewidth = 1,width = 0) +
  facet_wrap(~version+age, scales = "free", ncol = 5)+
  guides(
    color = guide_legend(position = "top"),
    shape = guide_legend(position = "top"))+
  scale_color_manual(name=NULL,values = c("red3","goldenrod","steelblue","olivedrab","orchid3"))+
  scale_shape_manual(name=NULL, values=c(16,17))+
  labs(x="Virus",y="Hospitalizations per 100,000")+
  theme(axis.text.x=element_blank())+
  geom_hline(yintercept=0)
LOO_hosp

ggsave(plot=LOO_hosp,"figures/figureS3.png",height=8.5,width=11,units="in")
ggsave(plot=LOO_hosp,"figures/TIFF/figureS3.tiff",height=8.5,width=11,units="in")


# Table Version -----------------------------------------------------------
table_version = rbind(hosp_avg,hosp_LOO_flu, hosp_LOO_covid, hosp_LOO_rsv,hosp_LOO_hmpv,hosp_LOO_rhino) %>% 
  pivot_longer(
    cols = matches("_(median|lower|upper)$"),
    names_to = c("virus", ".value"),
    names_sep = "_") %>% 
  filter(virus!="combo", model=="Ensemble") %>% 
  mutate(virus = factor(virus, levels=c("flu","rsv","hmpv","rhino","cov"),
                        labels=c("Influenza",'RSV',"HMPV","RV/EV","SARS-CoV-2")),
         age = factor(age, levels=c("<1","1to4","5to49","50to64","65+")),
         version = factor(version,levels=c("Reference","Leave out:Flu","Leave out:RSV","Leave out:HMPV","Leave out:RV/EV","Leave out:SARS-CoV-2"),
                          labels=c("Reference","Influenza","RSV","HMPV","RV/EV","SARS-CoV-2")),
         table_label = paste0(sprintf("%.1f", median),"\n(",sprintf("%.1f", lower),"-",sprintf("%.1f", upper),")")) %>% 
  arrange(age,virus, version) %>% 
  pivot_wider(id_cols=c(age,virus),names_from = version, values_from = table_label)

write_xlsx(table_version, "tables/tableS3.xlsx") 

 

