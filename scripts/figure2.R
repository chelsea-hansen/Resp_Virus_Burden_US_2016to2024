rm(list=ls())

library(tidyverse)
library(cowplot)
library(data.table)

'%notin%' = Negate("%in%")

source("R/summary_functions.R")


dat = readRDS("data/data_public.rds")
ages <- c("<1", "1to4", "5to49", "50to64", "65+")

hosp_model = readRDS("main_results/hosp_resp_any.rds") %>% 
  filter(date>='2016-07-01')

hosp_weekly = weekly_summary(hosp_model,"pop_enrolled") %>% 
  mutate(cause = "Respiratory (Any)")%>% 
  left_join(dat %>% select(date, "age"=agegrp, "obs"=hosp_resp_any_rate_smooth), by=c("date","age")) 

weekly_results = hosp_weekly %>% 
  mutate(excess_rhino = baseline_median + rhino_median,
         excess_flu   = excess_rhino + flu_median,
         excess_rsv   = excess_flu + rsv_median,
         excess_hmpv  = excess_rsv + hmpv_median,
         excess_cov   = excess_hmpv + cov_median,
         age = factor(age, levels=ages))


Fig2a = ggplot(weekly_results %>% filter(model=="Ensemble"))+
  theme_bw() +
  geom_area(aes(x=date, y=excess_cov,   fill="Excess COVID")) +
  geom_area(aes(x=date, y=excess_hmpv,  fill="Excess HMPV")) +
  geom_area(aes(x=date, y=excess_rsv,   fill="Excess RSV")) +
  geom_area(aes(x=date, y=excess_flu,   fill="Excess Flu")) +
  geom_area(aes(x=date, y=excess_rhino, fill="Excess RV")) +
  geom_area(aes(x=date, y=baseline_median, fill="Baseline")) +
  geom_line(aes(x=date, y=obs, color="Observed",group=1),
            linewidth=0.3) +
  scale_color_manual(name=NULL, values=c("black")) +
  scale_fill_manual(name=NULL,
                    values=c("seashell3","orchid3","red3","steelblue","goldenrod","olivedrab")) +
  facet_wrap(~age, scales="free", ncol=5) +
  theme(legend.position="top") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  labs(x=NULL, y="Hospitalizations per 100,000") 
Fig2a


mort_model = readRDS("main_results/mort_resp_any.rds") %>% 
  filter(date>='2016-07-01')

mort_weekly = weekly_summary(mort_model,"pop_total") %>% 
  left_join(dat %>% select(date, "age"=agegrp, "obs"=mort_resp_any_rate_smooth), by=c("date","age"))


weekly_results_mort_children = mort_weekly %>% 
  filter(age %in% c("<1","1to4")) %>% 
  mutate(excess_rhino = baseline_median + rhino_median,
         excess_flu   = excess_rhino + flu_median,
         excess_rsv   = excess_flu + rsv_median,
         excess_hmpv  = excess_rsv + hmpv_median,
         excess_cov   = excess_hmpv + cov_median,
         age = factor(age, levels=ages))

weekly_results_mort_adults = mort_weekly %>% 
  filter(age %in% c("5to49","50to64","65+")) %>% 
  mutate(baseline_median = baseline_median + rhino_median,
         excess_rhino=0,
         excess_flu   = baseline_median + flu_median,
    excess_rsv   = excess_flu + rsv_median,
    excess_hmpv  = excess_rsv + hmpv_median,
    excess_cov   = excess_hmpv + cov_median,
    age = factor(age, levels=ages))


weekly_results_mort = rbind(weekly_results_mort_children, weekly_results_mort_adults)

Fig2b = ggplot(weekly_results_mort %>% filter(model=="Ensemble"))+
  theme_bw() +
  geom_area(aes(x=date, y=excess_cov,   fill="Excess COVID")) +
  geom_area(aes(x=date, y=excess_hmpv,  fill="Excess HMPV")) +
  geom_area(aes(x=date, y=excess_rsv,   fill="Excess RSV")) +
  geom_area(aes(x=date, y=excess_flu,   fill="Excess Flu")) +
  geom_area(aes(x=date, y=excess_rhino, fill="Excess RV")) +
  geom_area(aes(x=date, y=baseline_median, fill="Baseline")) +
  geom_line(aes(x=date, y=obs, color="Observed",group=1),
            linewidth=0.3) +
  scale_color_manual(name=NULL, values=c("black")) +
  scale_fill_manual(name=NULL,
                    values=c("seashell3","orchid3","red3","steelblue","goldenrod","olivedrab")) +
  facet_wrap(~age, scales="free", ncol=5) +
  theme(legend.position="none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  labs(x=NULL, y="Deaths per 100,000") 
Fig2b

fig2 = plot_grid(Fig2a, Fig2b, nrow = 2, rel_heights = c(1,0.75))
fig2
ggsave(plot=fig2,"figures/Figure2.png",height=6,width=10,units='in')
ggsave(plot=fig2,"figures/TIFF/Figure2.tiff",height=6,width=10,units='in')



