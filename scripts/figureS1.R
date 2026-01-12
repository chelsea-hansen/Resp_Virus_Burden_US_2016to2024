rm(list=ls())

library(tidyverse)
library(cowplot)

'%notin%' = Negate("%in%")
source("R/summary_functions.R")

ages <- c("<1", "1to4", "5to49", "50to64", "65+")


seasons = c("2016-2017","2017-2018","2018-2019","2019-2020","2022-2023","2023-2024")
post_only = c("2022-2023","2023-2024")

hosp_model = readRDS("model_output/hosp_resp_any.rds") 
cov_avg = average_across_seasons(hosp_model, "pop_enrolled",post_only) %>% 
  select(model,age, cov_median,cov_lower,cov_upper)

hosp_avg = average_across_seasons(hosp_model, "pop_enrolled",seasons) %>% 
  select(-cov_median,-cov_lower,-cov_upper,-pop, -combo_median,-combo_lower,-combo_upper) %>% 
  left_join(cov_avg, by=c("model","age")) %>% 
  pivot_longer(cols=c(flu_median:cov_upper), names_to="name",values_to="estimate")%>%
  separate(name, into = c("virus", "measure"), sep = "_") %>% 
  mutate(age = factor(age, levels=c("<1","1to4","5to49","50to64","65+")),
         virus = factor(virus, levels=c("flu", "rsv", "hmpv", "rhino","cov"),
                        labels=c("Influenza","RSV","HMPV","RV","COVID-19")))%>% 
  pivot_wider(names_from=measure, values_from=estimate) %>% 
  mutate(model = factor(model,levels=c("Model_A","Model_B","Model_C","Model_D","Model_E","Ensemble"),
                        labels=c("A","B","C","D","E","Ensemble")))


figS1 = ggplot(hosp_avg %>% filter(model != "Ensemble")) +
  theme_bw() +
  # Background rectangle for Ensemble 95% CI
  geom_rect(
    data = hosp_avg %>% filter(model == "Ensemble"),
    aes(xmin = 0.5, xmax = 5.5, ymin = lower, ymax = upper, fill = "Ensemble 95% CI"),
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  # Horizontal line for Ensemble median
  geom_hline(
    data = hosp_avg %>% filter(model == "Ensemble"),
    aes(yintercept=median,color="Ensemble Median"),
    linewidth = 1
  ) +
  # Error bars and points for other models
  geom_errorbar(
    aes(x = model, ymin = lower, ymax = upper,color=model),
    width = 0, position = position_dodge(width = 0.5)
  ) +
  geom_point(
    aes(x = model, y = median,color=model),size=2,
    position = position_dodge(width = 0.5)
  ) +
  # Scales
  scale_fill_manual(name=NULL,values = c("Ensemble 95% CI"="grey")) +
  scale_color_manual(name="Model",
    values = c(
      "A"="red",
      "B"="goldenrod2",
      "C"="steelblue",
      "D"="olivedrab",
      "E"="darkorchid",
      "Ensemble Median"="black"
    )
  ) +
  # Facets
  facet_wrap(~virus + age, scales = "free_y", ncol = 5) +
  # Labels
  labs(x = "Model", y = "Hospitalizations per 100,000") +
  # Theme
  theme(
   # legend.position = "top",
   # legend.direction = "horizontal",
    strip.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )
  figS1
ggsave(plot=figS1,"figures/figureS1.png",height=7.25,width=12,units="in")
ggsave(plot=figS1,"figures/TIFF/figureS1.tiff",height=7.25,width=12,units="in")