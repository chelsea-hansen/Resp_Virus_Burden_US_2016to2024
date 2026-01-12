compare_pre_and_post = function(model_output, pop_col){
  
  
  pre_pandemic = c("2016-2017","2017-2018","2018-2019","2019-2020")
  post_pandemic = c("2021-2022","2022-2023","2023-2024")
  post_pandemic_covid = c("2022-2023","2023-2024")
  
  pop_pre = model_output %>%filter(season %in% pre_pandemic) %>%  group_by(age) %>% summarize(true_pop = mean(pop_total)) %>% mutate(prop = true_pop/sum(true_pop))
  pop_post = model_output %>%filter(season %in% post_pandemic_covid) %>%  group_by(age) %>% summarize(true_pop = mean(pop_total)) %>% mutate(prop = true_pop/sum(true_pop))
  
  
  pre_summary = average_across_seasons(model_output,pop_col,pre_pandemic) %>% mutate(period="before") %>% 
    left_join(pop_pre,by="age") %>% 
    select(-cov_median, -cov_lower,-cov_upper)
  
  pre_summary_all = pre_summary %>% group_by(model) %>% 
    summarize(flu_median = weighted.mean(flu_median,prop),
              flu_lower = weighted.mean(flu_lower,prop),
              flu_upper = weighted.mean(flu_upper,prop),
              rsv_median = weighted.mean(rsv_median,prop),
              rsv_lower = weighted.mean(rsv_lower,prop),
              rsv_upper = weighted.mean(rsv_upper,prop),
              hmpv_median = weighted.mean(hmpv_median,prop),
              hmpv_lower = weighted.mean(hmpv_lower,prop),
              hmpv_upper = weighted.mean(hmpv_upper,prop),
              rhino_median = weighted.mean(rhino_median,prop),
              rhino_lower = weighted.mean(rhino_lower,prop),
              rhino_upper = weighted.mean(rhino_upper,prop),
              combo_median = weighted.mean(combo_median,prop),
              combo_lower = weighted.mean(combo_lower,prop),
              combo_upper = weighted.mean(combo_upper,prop),
              true_pop = sum(true_pop)) %>% 
    mutate(age="All",period="before")
  
  
  #post_summary_cov = average_across_seasons(model_output,pop_col,post_pandemic_covid)%>% mutate(period="post") %>% 
   # left_join(pop_post,by="age") %>%
   # select(model, age, period, true_pop, prop, cov_median, cov_lower,cov_upper)
  
 # post_summary_all_cov = post_summary_cov %>% group_by(model) %>% 
  #  summarize(cov_median = weighted.mean(cov_median,prop),
             # cov_lower = weighted.mean(cov_lower,prop),
             # cov_upper = weighted.mean(cov_upper,prop))
  
  
post_summary = average_across_seasons(model_output,pop_col,post_pandemic_covid)%>% mutate(period="post") %>% 
    left_join(pop_post,by="age") #%>%
    #select(-cov_median, -cov_lower,-cov_upper) %>% 
    #left_join(post_summary_cov %>% select(-prop,-true_pop), by=c("model","period","age"))
  
  
  post_summary_all = post_summary %>% group_by(model) %>% 
    summarize(flu_median = weighted.mean(flu_median,prop),
              flu_lower = weighted.mean(flu_lower,prop),
              flu_upper = weighted.mean(flu_upper,prop),
              rsv_median = weighted.mean(rsv_median,prop),
              rsv_lower = weighted.mean(rsv_lower,prop),
              rsv_upper = weighted.mean(rsv_upper,prop),
              hmpv_median = weighted.mean(hmpv_median,prop),
              hmpv_lower = weighted.mean(hmpv_lower,prop),
              hmpv_upper = weighted.mean(hmpv_upper,prop),
              rhino_median = weighted.mean(rhino_median,prop),
              rhino_lower = weighted.mean(rhino_lower,prop),
              rhino_upper = weighted.mean(rhino_upper,prop),
              cov_median = weighted.mean(cov_median,prop),
              cov_lower = weighted.mean(cov_lower,prop),
              cov_upper = weighted.mean(cov_upper,prop),
              combo_median = weighted.mean(combo_median,prop),
              combo_lower = weighted.mean(combo_lower,prop),
              combo_upper = weighted.mean(combo_upper,prop),
              true_pop = sum(true_pop)) %>% 
    #left_join(post_summary_all_cov, by=c("model")) %>% 
    mutate(age="All",period="post")
  
  
  
  table = bind_rows(pre_summary, pre_summary_all, post_summary,post_summary_all) %>% 
    filter(model=="Ensemble") %>% 
    mutate(flu_count = round(flu_median/100000*true_pop),
           flu_count_lwr = round(flu_lower/100000*true_pop),
           flu_count_upr = round(flu_upper/100000*true_pop),
           rsv_count = round(rsv_median/100000*true_pop),
           rsv_count_lwr = round(rsv_lower/100000*true_pop),
           rsv_count_upr = round(rsv_upper/100000*true_pop),
           hmpv_count = round(hmpv_median/100000*true_pop),
           hmpv_count_lwr = round(hmpv_lower/100000*true_pop),
           hmpv_count_upr = round(hmpv_upper/100000*true_pop),
           rhino_count = round(rhino_median/100000*true_pop),
           rhino_count_lwr = round(rhino_lower/100000*true_pop),
           rhino_count_upr = round(rhino_upper/100000*true_pop),
           covid_count = round(cov_median/100000*true_pop),
           covid_count_lwr = round(cov_lower/100000*true_pop),
           covid_count_upr = round(cov_upper/100000*true_pop),
           combo_count = round(combo_median/100000*true_pop),
           combo_count_lwr = round(combo_lower/100000*true_pop),
           combo_count_upr = round(combo_upper/100000*true_pop),
           flu_rate = paste0(sprintf("%.1f", flu_median),"\n(",sprintf("%.1f", flu_lower),"-",sprintf("%.1f", flu_upper),")"),
           rsv_rate = paste0(sprintf("%.1f", rsv_median),"\n(",sprintf("%.1f", rsv_lower),"-",sprintf("%.1f", rsv_upper),")"),
           hmpv_rate = paste0(sprintf("%.1f", hmpv_median),"\n(",sprintf("%.1f", hmpv_lower),"-",sprintf("%.1f", hmpv_upper),")"),
           rhino_rate = paste0(sprintf("%.1f", rhino_median),"\n(",sprintf("%.1f", rhino_lower),"-",sprintf("%.1f", rhino_upper),")"),
           covid_rate = paste0(sprintf("%.1f", cov_median),"\n(",sprintf("%.1f", cov_lower),"-",sprintf("%.1f", cov_upper),")"),
           combo_rate = paste0(sprintf("%.1f", combo_median),"\n(",sprintf("%.1f", combo_lower),"-",sprintf("%.1f", combo_upper),")"),
           flu_count = paste0(flu_count,"\n(", flu_count_lwr,"-",flu_count_upr,")"),
           rsv_count = paste0(rsv_count,"\n(", rsv_count_lwr,"-",rsv_count_upr,")"),
           hmpv_count = paste0(hmpv_count,"\n(", hmpv_count_lwr,"-",hmpv_count_upr,")"),
           rhino_count = paste0(rhino_count,"\n(", rhino_count_lwr,"-",rhino_count_upr,")"),
           covid_count = paste0(covid_count,"\n(", covid_count_lwr,"-",covid_count_upr,")"),
           combo_count = paste0(combo_count,"\n(", combo_count_lwr,"-",combo_count_upr,")")) %>% 
    select(age,period, flu_rate,flu_count, rsv_rate,rsv_count, hmpv_rate,hmpv_count, rhino_rate,rhino_count,
           covid_rate, covid_count, combo_rate,combo_count) %>% 
    mutate(age = factor(age,levels=c("<1","1to4","5to49","50to64","65+","All"))) %>% 
    arrange(period, age)
  
  return(table)
}

compare_models = function(model_output, seasons, virus, pop_col){
  
  med_col   <- sym(paste0(virus, "_median"))
  lower_col <- sym(paste0(virus, "_lower"))
  upper_col <- sym(paste0(virus, "_upper"))
  
  
  pop_prop = model_output %>% 
    filter(season %in% seasons) %>% 
    group_by(age) %>% 
    summarize(true_pop = mean(pop_total)) %>% mutate(prop = true_pop/sum(true_pop))
  
  avg = average_across_seasons(model_output,pop_col,seasons) %>% 
    left_join(pop_prop, by="age") %>% 
    select(model, age, prop, true_pop,"median"=!!med_col, "lower"=!!lower_col, "upper"=!!upper_col)
  
  
  
  all = avg %>% group_by(model) %>% 
    summarize(median = weighted.mean(median,prop),
              lower = weighted.mean(lower,prop),
              upper = weighted.mean(upper,prop),
              true_pop = sum(true_pop)) %>% 
    mutate(age="All")
  
  
  
  table = bind_rows(avg, all) %>% 
    mutate(count = round(median/100000*true_pop),
           count_lwr = round(lower/100000*true_pop),
           count_upr = round(upper/100000*true_pop),
           rate = paste0(sprintf("%.1f", median)," (",sprintf("%.1f", lower),"-",sprintf("%.1f", upper),")"),
           count = paste0(count," (",count_lwr,"-",count_upr,")")) %>% 
    select(model,age,rate, count) %>% 
    mutate(age = factor(age,levels=c("<1","1to4","5to49","50to64","65+","All"))) %>% 
    pivot_wider(names_from=model, values_from = c(rate, count)) 
  
 return(table) 
 
}

average_table = function(model_output, pop_col, season_list){
 
  covid_seasons = c("2022-2023","2023-2024")
 pop = model_output %>%  group_by(age) %>% summarize(true_pop = mean(pop_total)) %>% mutate(prop = true_pop/sum(true_pop))
  

  
  get_averages = average_across_seasons(model_output,pop_col,season_list) %>%
    left_join(pop,by="age") %>% 
    select(-cov_median, -cov_lower,-cov_upper)
  
  get_averages_all = get_averages %>% group_by(model) %>% 
    summarize(flu_median = weighted.mean(flu_median,prop),
              flu_lower = weighted.mean(flu_lower,prop),
              flu_upper = weighted.mean(flu_upper,prop),
              rsv_median = weighted.mean(rsv_median,prop),
              rsv_lower = weighted.mean(rsv_lower,prop),
              rsv_upper = weighted.mean(rsv_upper,prop),
              hmpv_median = weighted.mean(hmpv_median,prop),
              hmpv_lower = weighted.mean(hmpv_lower,prop),
              hmpv_upper = weighted.mean(hmpv_upper,prop),
              rhino_median = weighted.mean(rhino_median,prop),
              rhino_lower = weighted.mean(rhino_lower,prop),
              rhino_upper = weighted.mean(rhino_upper,prop),
              combo_median = weighted.mean(combo_median,prop),
              combo_lower = weighted.mean(combo_lower,prop),
              combo_upper = weighted.mean(combo_upper,prop),
              true_pop = sum(true_pop)) %>% 
    mutate(age="All")
  
  
  get_covid = average_across_seasons(model_output,pop_col,covid_seasons) %>% 
   left_join(pop,by="age") %>%
   select(model, age, true_pop, prop, cov_median, cov_lower,cov_upper)
  
   get_covid_all = get_covid %>% group_by(model) %>% 
    summarize(cov_median = weighted.mean(cov_median,prop),
   cov_lower = weighted.mean(cov_lower,prop),
   cov_upper = weighted.mean(cov_upper,prop),
   true_pop = sum(true_pop))%>% 
     mutate(age="All")
   
  
  get_averages = get_averages %>% 
    left_join(get_covid, by=c("age","model","true_pop","prop"))

  get_averages_all = get_averages_all %>% 
    left_join(get_covid_all, by=c("age","model","true_pop"))
  
  table = bind_rows(get_averages, get_averages_all) %>% 
    filter(model=="Ensemble") %>% 
    mutate(flu_count = round(flu_median/100000*true_pop),
           flu_count_lwr = round(flu_lower/100000*true_pop),
           flu_count_upr = round(flu_upper/100000*true_pop),
           rsv_count = round(rsv_median/100000*true_pop),
           rsv_count_lwr = round(rsv_lower/100000*true_pop),
           rsv_count_upr = round(rsv_upper/100000*true_pop),
           hmpv_count = round(hmpv_median/100000*true_pop),
           hmpv_count_lwr = round(hmpv_lower/100000*true_pop),
           hmpv_count_upr = round(hmpv_upper/100000*true_pop),
           rhino_count = round(rhino_median/100000*true_pop),
           rhino_count_lwr = round(rhino_lower/100000*true_pop),
           rhino_count_upr = round(rhino_upper/100000*true_pop),
           covid_count = round(cov_median/100000*true_pop),
           covid_count_lwr = round(cov_lower/100000*true_pop),
           covid_count_upr = round(cov_upper/100000*true_pop),
           combo_count = round(combo_median/100000*true_pop),
           combo_count_lwr = round(combo_lower/100000*true_pop),
           combo_count_upr = round(combo_upper/100000*true_pop),
           flu_rate = paste0(sprintf("%.1f", flu_median),"\n(",sprintf("%.1f", flu_lower),"-",sprintf("%.1f", flu_upper),")"),
           rsv_rate = paste0(sprintf("%.1f", rsv_median),"\n(",sprintf("%.1f", rsv_lower),"-",sprintf("%.1f", rsv_upper),")"),
           hmpv_rate = paste0(sprintf("%.1f", hmpv_median),"\n(",sprintf("%.1f", hmpv_lower),"-",sprintf("%.1f", hmpv_upper),")"),
           rhino_rate = paste0(sprintf("%.1f", rhino_median),"\n(",sprintf("%.1f", rhino_lower),"-",sprintf("%.1f", rhino_upper),")"),
           covid_rate = paste0(sprintf("%.1f", cov_median),"\n(",sprintf("%.1f", cov_lower),"-",sprintf("%.1f", cov_upper),")"),
           combo_rate = paste0(sprintf("%.1f", combo_median),"\n(",sprintf("%.1f", combo_lower),"-",sprintf("%.1f", combo_upper),")"),
           flu_count = paste0(flu_count,"\n(", flu_count_lwr,"-",flu_count_upr,")"),
           rsv_count = paste0(rsv_count,"\n(", rsv_count_lwr,"-",rsv_count_upr,")"),
           hmpv_count = paste0(hmpv_count,"\n(", hmpv_count_lwr,"-",hmpv_count_upr,")"),
           rhino_count = paste0(rhino_count,"\n(", rhino_count_lwr,"-",rhino_count_upr,")"),
           covid_count = paste0(covid_count,"\n(", covid_count_lwr,"-",covid_count_upr,")"),
           combo_count = paste0(combo_count,"\n(", combo_count_lwr,"-",combo_count_upr,")")) %>% 
    select(age,flu_rate,flu_count, rsv_rate,rsv_count, hmpv_rate,hmpv_count, rhino_rate,rhino_count,
           covid_rate, covid_count, combo_rate,combo_count) %>% 
    mutate(age = factor(age,levels=c("<1","1to4","5to49","50to64","65+","All"))) %>% 
    arrange(age)
  
  return(table)
  
}
