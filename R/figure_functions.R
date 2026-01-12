burden_time_series = function(data1, y_value, data2, label){
  
  data1 <- data1 %>%
    mutate(obs_value = .[[y_value]],
           age=agegrp)
  
  plot_data = data2 %>% 
    mutate(excess_rhino = baseline_median + rhino_median,
           excess_flu   = excess_rhino + flu_median,
           excess_rsv   = excess_flu + rsv_median,
           excess_hmpv  = excess_rsv + hmpv_median,
           excess_cov   = excess_hmpv + cov_median,
           age = factor(age, levels=c("<1","1to4","5to49","50to64","65+")))
  
  #minimums = data1 %>% 
  ##  group_by(agegrp) %>% 
  #  summarize(min_observed = min(obs_value), .groups = "drop") %>% 
   # mutate(age = factor(agegrp, levels=c("<1","1to4","5to49","50to64","65+"))) %>% 
   # arrange(age)
  
 # mins = minimums$min_observed * 0.8
  
  figure = ggplot() +
    theme_bw() +
    geom_area(data=plot_data ,aes(x=date, y=excess_cov,   fill="Excess COVID")) +
    geom_area(data=plot_data ,aes(x=date, y=excess_hmpv,  fill="Excess hMPV")) +
    geom_area(data=plot_data,aes(x=date, y=excess_rsv,   fill="Excess RSV")) +
    geom_area(data=plot_data ,aes(x=date, y=excess_flu,   fill="Excess Flu")) +
    geom_area(data=plot_data ,aes(x=date, y=excess_rhino, fill="Excess RV")) +
    geom_area(data=plot_data ,aes(x=date, y=baseline_median, fill="Baseline")) +
    geom_line(data = data1,
              aes(x=date, y=obs_value, color="Observed",group=1),
              linewidth=0.3) +
    scale_color_manual(name=NULL, values=c("black")) +
    scale_fill_manual(name=NULL,
                      values=c("seashell3","orchid3","red3","steelblue","goldenrod","olivedrab")) +
    facet_wrap(~model+age, scales="free", ncol=5) +
    theme(legend.position="top") +
    labs(x=NULL, y=label) #+
   # facetted_pos_scales(
    #  y = list(
   #     age == "<1"    ~ scale_y_continuous(limits = c(mins[1], NA)),
    #    age == "1to4"  ~ scale_y_continuous(limits = c(mins[2], NA)),
    #    age == "5to49" ~ scale_y_continuous(limits = c(mins[3], NA)),
   #     age == "50to64"~ scale_y_continuous(limits = c(mins[4], NA)),
   #     age == "65+"   ~ scale_y_continuous(limits = c(mins[5], NA))
   #   )
  #  )
  
  return(figure)
}


plot_ensemble_with_models = function(data, label, virus){
  
  med_col   <- sym(paste0(virus, "_median"))
  lower_col <- sym(paste0(virus, "_lower"))
  upper_col <- sym(paste0(virus, "_upper"))
 
   figure = ggplot() +
    theme_bw() +
    geom_errorbar(data = data %>% filter(model!="Ensemble"), aes(x = season, ymin = !!lower_col, ymax = !!upper_col, color = model),
                  width = 0, linewidth = 1, alpha = 0.5) +
    geom_point(data = data %>% filter(model!="Ensemble"), aes(x = season, y = !!med_col, color = model), size = 1) +
    geom_errorbar(data = data %>% filter(model=="Ensemble"), aes(x = season, ymin = !!lower_col, ymax = !!upper_col, color="Ensemble"), width = 0.2) +
    geom_point(data = data %>% filter(model=="Ensemble"), aes(x = season, y = !!med_col, color="Ensemble"), size = 3) +
    facet_wrap(~cause+age, scales = 'free_y',ncol=5) +
    theme(axis.text.x = element_text(angle=90),
          legend.position="top",legend.direction="horizontal") +
    guides(color = guide_legend(nrow = 1))+
    scale_color_manual(name="Model",values=c("black","#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3"))+
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = NULL, y = paste0("Excess ", label ," per 100,000"))
  
  return(figure)
}

plot_ensemble_with_recorded = function(mod_data,recorded_data,outcome,virus,label){
 
  if(virus=="flu"){
    color1 = "red"
    color2 = "lightsalmon"
  }else if(virus=="rsv"){
    color1 = "darkgoldenrod"
    color2 = "goldenrod2"
  } else if(virus=="hmpv"){
    color1 = "steelblue4"
    color2 = "slategray2"
  }else if(virus=="rhino"){
    color1 = "darkolivegreen4"
    color2 = "olivedrab2"
  }else if(virus=="cov"){
    color1 = "darkorchid4"
    color2 = "orchid1"
  }
  
  
  
  
  med_col   <- sym(paste0(virus, "_median"))
  lower_col <- sym(paste0(virus, "_lower"))
  upper_col <- sym(paste0(virus, "_upper"))
  uc_col <- sym(paste0(virus, "_uc_rate"))
  mc_col <- sym(paste0(virus, "_mc_rate"))
  any_col <- sym(paste0(virus, "_any_rate"))
  first_or_second_col <- sym(paste0(virus, "_1or2_rate"))
  
  plot_data = mod_data %>% filter(model=="Ensemble")
  season_levels <- unique(plot_data$season)
  plot_data$season_num <- match(plot_data$season, season_levels)
  
  if(outcome=="mortality"){
    figure = ggplot() +
      theme_bw() +
      geom_tile(
        data = plot_data,
        aes(
          x = season_num,
          y = (!!upper_col + !!lower_col) / 2,  
          height = !!upper_col - !!lower_col,    
          width = 1,                        
          fill = "Ensemble Model"
        ),
        alpha = 0.5
      ) +
      geom_point(
        data = plot_data,
        aes(x = season_num, y = !!med_col, color = "Ensemble Model"),
        size = 5, shape = 15
      ) +
      geom_point(
        data = recorded_data,
        aes(x = match(season, season_levels), y = !!mc_col, color = "Multiple Cause"),
        size = 3
      ) +
      geom_point(
        data = recorded_data,
        aes(x = match(season, season_levels), y = !!uc_col, color = "Underlying Cause"),
        size = 3
      ) +
      scale_x_continuous(
        breaks = seq_along(season_levels),
        labels = season_levels,
        expand = c(0, 0)
      ) +
      scale_fill_manual(values = c("Ensemble Model" = "seashell3"), name = NULL) +
      scale_color_manual(values = c("Ensemble Model" = "seashell4", "Multiple Cause" = color1, "Underlying Cause" = color2), name = NULL) +
      facet_wrap(~cause+age, scales = "free_y", ncol = 5) +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "top") +
      guides(fill="none")+
      labs(x = NULL, y = paste0(label," per 100,000"))
    
    
  }else{
    figure = ggplot() +
      theme_bw() +
      geom_tile(
        data = plot_data,
        aes(
          x = season_num,
          y = (!!upper_col + !!lower_col) / 2,  
          height = !!upper_col - !!lower_col,    
          width = 1,                        
          fill = "Ensemble Model"
        ),
        alpha = 0.5
      ) +
      geom_point(
        data = plot_data,
        aes(x = season_num, y = !!med_col, color = "Ensemble Model"),
        size = 5, shape = 15
      ) +
      geom_point(
        data = recorded_data,
        aes(x = match(season, season_levels), y = !!any_col, color = "Any Position"),
        size = 3
      ) +
      geom_point(
        data = recorded_data,
        aes(x = match(season, season_levels), y = !!first_or_second_col, color = "1st or 2nd Position"),
        size = 3
      ) +
      scale_x_continuous(
        breaks = seq_along(season_levels),
        labels = season_levels,
        expand = c(0, 0)
      ) +
      scale_fill_manual(values = c("Ensemble Model" = "seashell3"), name = NULL) +
      scale_color_manual(breaks=c("Ensemble Model","Any Position","1st or 2nd Position"),values = c("Ensemble Model" = "seashell4", "Any Position" = color1, "1st or 2nd Position" = color2), name = NULL) +
      facet_wrap(~cause+age, scales = "free_y", ncol = 5) +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "top") +
      guides(fill="none")+
      labs(x = NULL, y = paste0(label," per 100,000"))
    
    
  }
  return(figure)
 
}
  plot_ensemble_with_recorded_v2 = function(mod_data,recorded_data,virus,label){
  
  if(virus=="flu"){
    color1 = "red"
    color2 = "lightsalmon"
  }else if(virus=="rsv"){
    color1 = "darkgoldenrod"
    color2 = "goldenrod2"
  } else if(virus=="hmpv"){
    color1 = "steelblue4"
    color2 = "slategray2"
  }else if(virus=="rhino"){
    color1 = "darkolivegreen4"
    color2 = "olivedrab2"
  }else if(virus=="cov"){
    color1 = "darkorchid4"
    color2 = "orchid1"
  }
  
  
  
  
  med_col   <- sym(paste0(virus, "_median"))
  lower_col <- sym(paste0(virus, "_lower"))
  upper_col <- sym(paste0(virus, "_upper"))
  uc_col <- sym(paste0(virus, "_uc_rate"))
  mc_col <- sym(paste0(virus, "_mc_rate"))
  any_col <- sym(paste0(virus, "_any_rate"))
  first_or_second_col <- sym(paste0(virus, "_1or2_rate"))
  
  plot_data = mod_data %>% filter(model=="Ensemble")
  season_levels <- unique(plot_data$season)
  plot_data$season_num <- match(plot_data$season, season_levels)
  
  
  plot1 = ggplot() +
    theme_bw() +
    geom_tile(
      data = plot_data %>% filter(cause=="Mortality"),
      aes(
        x = season_num,
        y = (!!upper_col + !!lower_col) / 2,  
        height = !!upper_col - !!lower_col,    
        width = 0.8,                        
        fill = "Ensemble Model"
      ),
      alpha = 0.5
    ) +
    geom_point(
      data = plot_data%>% filter(cause=="Mortality"),
      aes(x = season_num, y = !!med_col, color = "Ensemble Model"),
      size = 3, shape = 15
    ) +
    geom_point(
      data = recorded_data,
      aes(x = match(season, season_levels), y = !!mc_col, color = "Multiple Cause"),
      size = 1.5
    ) +
    geom_point(
      data = recorded_data,
      aes(x = match(season, season_levels), y = !!uc_col, color = "Underlying Cause"),
      size = 1.5
    ) +
    scale_x_continuous(
      breaks = seq_along(season_levels),
      labels = season_levels,
      expand = c(0, 0)
    ) +
    scale_fill_manual(values = c("Ensemble Model" = "seashell3"), name = NULL) +
    scale_color_manual(values = c("Ensemble Model" = "seashell4", "Multiple Cause" = color1, "Underlying Cause" = color2), name = NULL) +
    facet_wrap(~cause + age, scales = "free_y", ncol = 5) +
    theme(axis.text.x = element_text(angle = 90)) +
    guides(fill="none")+
    labs(x = NULL, y = paste0(label," per 100,000"))
plot1

  
  plot2 = ggplot() +
    theme_bw() +
    geom_tile(
      data = plot_data %>% filter(cause=="Hospitalization"),
      aes(
        x = season_num,
        y = (!!upper_col + !!lower_col) / 2,  
        height = !!upper_col - !!lower_col,    
        width = 0.8,                        
        fill = "Ensemble Model"
      ),
      alpha = 0.5
    ) +
    geom_point(
      data = plot_data%>% filter(cause=="Hospitalization"),
      aes(x = season_num, y = !!med_col, color = "Ensemble Model"),
      size = 3, shape = 15
    ) +
    geom_point(
      data = recorded_data,
      aes(x = match(season, season_levels), y = !!any_col, color = "Any Position"),
      size = 1.5
    ) +
    geom_point(
      data = recorded_data,
      aes(x = match(season, season_levels), y = !!first_or_second_col, color = "1st or 2nd Position"),
      size = 1.5
    ) +
    scale_x_continuous(
      breaks = seq_along(season_levels),
      labels = season_levels,
      expand = c(0, 0)
    ) +
    scale_fill_manual(values = c("Ensemble Model" = "seashell3"), name = NULL) +
    scale_color_manual(breaks=c("Ensemble Model","Any Position","1st or 2nd Position"),values = c("Ensemble Model" = "seashell4", "Any Position" = color1, "1st or 2nd Position" = color2), name = NULL) +
    facet_wrap(~cause+age, scales = "free_y", ncol = 5) +
    theme(axis.text.x = element_blank()) +
    guides(fill="none")+
    labs(x = NULL, y = paste0(label," per 100,000"))
  
  figure= plot_grid(plot2, plot1, nrow=2, rel_heights = c(0.8,1), align="v", labels="AUTO")
  
  return(figure)
  
}

plot_ensemble_with_recorded_v3 = function(mod_data,recorded_data,virus1,label1,virus2,  label2){
  
  if(virus1=="flu"){
    color1.1 = "red"
    color2.1 = "lightsalmon"
  }else if(virus1=="rsv"){
    color1.1 = "darkgoldenrod"
    color2.1 = "goldenrod2"
  } else if(virus1=="hmpv"){
    color1.1 = "steelblue4"
    color2.1 = "slategray2"
  }else if(virus1=="rhino"){
    color1.1 = "darkolivegreen4"
    color2.1 = "olivedrab2"
  }else if(virus1=="cov"){
    color1.1 = "darkorchid4"
    color2.1 = "orchid1"
  }
  
  if(virus2=="flu"){
    color1.2 = "red"
    color2.2 = "lightsalmon"
  }else if(virus2=="rsv"){
    color1.2 = "darkgoldenrod"
    color2.2 = "goldenrod2"
  } else if(virus2=="hmpv"){
    color1.2 = "steelblue4"
    color2.2 = "slategray2"
  }else if(virus2=="rhino"){
    color1.2 = "darkolivegreen4"
    color2.2 = "olivedrab2"
  }else if(virus2=="cov"){
    color1.2 = "darkorchid4"
    color2.2 = "orchid1"
  }
  
  
  
  med_col1   <- sym(paste0(virus1, "_median"))
  lower_col1 <- sym(paste0(virus1, "_lower"))
  upper_col1 <- sym(paste0(virus1, "_upper"))
  any_col1 <- sym(paste0(virus1, "_any_rate"))
  first_or_second_col1 <- sym(paste0(virus1, "_1or2_rate"))
  
  med_col2   <- sym(paste0(virus2, "_median"))
  lower_col2 <- sym(paste0(virus2, "_lower"))
  upper_col2 <- sym(paste0(virus2, "_upper"))
  any_col2 <- sym(paste0(virus2, "_any_rate"))
  first_or_second_col2 <- sym(paste0(virus2, "_1or2_rate"))
  
  
  plot_data = mod_data %>% filter(model=="Ensemble")
  season_levels <- unique(plot_data$season)
  plot_data$season_num <- match(plot_data$season, season_levels)
  
  
  plot1 = ggplot() +
    theme_bw() +
    geom_tile(
      data = plot_data,
      aes(
        x = season_num,
        y = (!!upper_col1 + !!lower_col1) / 2,  
        height = !!upper_col1 - !!lower_col1,    
        width = 0.8,                        
        fill = "Ensemble Model"
      ),
      alpha = 0.5
    ) +
    geom_point(
      data = plot_data,
      aes(x = season_num, y = !!med_col1, color = "Ensemble Model"),
      size = 3, shape = 15
    ) +
    geom_point(
      data = recorded_data,
      aes(x = match(season, season_levels), y = !!any_col1, color = "Any Position"),
      size = 1.5
    ) +
    geom_point(
      data = recorded_data,
      aes(x = match(season, season_levels), y = !!first_or_second_col1, color = "1st or 2nd Position"),
      size = 1.5
    ) +
    scale_x_continuous(
      breaks = seq_along(season_levels),
      labels = season_levels,
      expand = c(0, 0)
    ) +
    scale_fill_manual(values = c("Ensemble Model" = "seashell3"), name = NULL) +
    scale_color_manual(breaks=c("Ensemble Model","Any Position","1st or 2nd Position"),values = c("Ensemble Model" = "seashell4", "Any Position" = color1.1, "1st or 2nd Position" = color2.1), name = NULL) +
    facet_wrap(~age, scales = "free_y", ncol = 5) +
    theme(axis.text.x = element_blank()) +
    guides(fill="none")+
    labs(x = NULL, y = paste0(label1," per 100,000"))
  
  
  plot2 = ggplot() +
    theme_bw() +
    geom_tile(
      data = plot_data,
      aes(
        x = season_num,
        y = (!!upper_col2 + !!lower_col2) / 2,  
        height = !!upper_col2 - !!lower_col2,    
        width = 0.8,                        
        fill = "Ensemble Model"
      ),
      alpha = 0.5
    ) +
    geom_point(
      data = plot_data,
      aes(x = season_num, y = !!med_col2, color = "Ensemble Model"),
      size = 3, shape = 15
    ) +
    geom_point(
      data = recorded_data,
      aes(x = match(season, season_levels), y = !!any_col2, color = "Any Position"),
      size = 1.5
    ) +
    geom_point(
      data = recorded_data,
      aes(x = match(season, season_levels), y = !!first_or_second_col2, color = "1st or 2nd Position"),
      size = 1.5
    ) +
    scale_x_continuous(
      breaks = seq_along(season_levels),
      labels = season_levels,
      expand = c(0, 0)
    ) +
    scale_fill_manual(values = c("Ensemble Model" = "seashell3"), name = NULL) +
    scale_color_manual(breaks=c("Ensemble Model","Any Position","1st or 2nd Position"),values = c("Ensemble Model" = "seashell4", "Any Position" = color1.2, "1st or 2nd Position" = color2.2), name = NULL) +
    facet_wrap(~age, scales = "free_y", ncol = 5) +
    theme(axis.text.x = element_text(angle = 90)) +
    guides(fill="none")+
    labs(x = NULL, y = paste0(label2," per 100,000"))
  
  figure= plot_grid(plot1, plot2, nrow=2, rel_heights = c(0.8,1), align="v", labels="AUTO")
  
  return(figure)
  
}
