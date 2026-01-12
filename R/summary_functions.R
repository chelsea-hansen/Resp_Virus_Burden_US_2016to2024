

# Function for creating weekly-level ensemble -----------------------------
weekly_summary = function(data, pop_col){
  
  data <- as.data.table(data)
  pop_col <- as.name(pop_col) 
  
  bootstrap <- data[iteration>0,.(
    flu_median = quantile(flu_ex, 0.50, na.rm = TRUE),
    rsv_median = quantile(rsv_ex, 0.50, na.rm = TRUE),
    hmpv_median = quantile(hmpv_ex, 0.50, na.rm = TRUE),
    rhino_median = quantile(rhino_ex, 0.50, na.rm = TRUE),
    cov_median = quantile(cov_ex, 0.50, na.rm = TRUE),
    baseline_median = quantile(baseline, 0.5, na.rm = TRUE),
    pop = mean(eval(pop_col), na.rm = TRUE)
  ), by = .(model, age, date)]
  
  ensemble <- bootstrap[
    , .(
      flu_median = mean(flu_median),
      rsv_median = mean(rsv_median),
      hmpv_median = mean(hmpv_median),
      rhino_median = mean(rhino_median),
      cov_median = mean(cov_median),
      baseline_median = mean(baseline_median),
      pop = mean(pop)
    ),
    by = .(age, date)
  ][
    , model := "Ensemble"
  ]
  
  results = rbind(bootstrap, ensemble)
  return(results)
}


# Function for creating season-level ensemble  ----------------------------
seasonal_summary = function(data, pop_col){
  
  data <- as.data.table(data)
  pop_col <- as.name(pop_col) 
  
  bootstrap <- data[iteration>0, .(
    flu_excess = sum(flu_ex, na.rm = TRUE),
    rsv_excess = sum(rsv_ex, na.rm = TRUE),
    hmpv_excess = sum(hmpv_ex, na.rm = TRUE),
    rhino_excess = sum(rhino_ex, na.rm = TRUE),
    cov_excess = sum(cov_ex, na.rm = TRUE),
    combo_excess = sum(flu_ex + rsv_ex + hmpv_ex + rhino_ex + cov_ex,na.rm=TRUE),
    baseline = sum(baseline, na.rm = TRUE),
    pop = mean(eval(pop_col), na.rm = TRUE)
  ), by = .(model, age, iteration, season)][,.(
    flu_median = quantile(flu_excess, 0.50, na.rm = TRUE),
    flu_lower = quantile(flu_excess, 0.025, na.rm = TRUE),
    flu_upper = quantile(flu_excess, 0.975, na.rm = TRUE),
    rsv_median = quantile(rsv_excess, 0.50, na.rm = TRUE),
    rsv_lower = quantile(rsv_excess, 0.025, na.rm = TRUE),
    rsv_upper = quantile(rsv_excess, 0.975, na.rm = TRUE),
    hmpv_median = quantile(hmpv_excess, 0.50, na.rm = TRUE),
    hmpv_lower = quantile(hmpv_excess, 0.025, na.rm = TRUE),
    hmpv_upper = quantile(hmpv_excess, 0.975, na.rm = TRUE),
    rhino_median = quantile(rhino_excess, 0.50, na.rm = TRUE),
    rhino_lower = quantile(rhino_excess, 0.025, na.rm = TRUE),
    rhino_upper = quantile(rhino_excess, 0.975, na.rm = TRUE),
    cov_median = quantile(cov_excess, 0.50, na.rm = TRUE),
    cov_lower = quantile(cov_excess, 0.025, na.rm = TRUE),
    cov_upper = quantile(cov_excess, 0.975, na.rm = TRUE),
    combo_median = quantile(combo_excess, 0.50, na.rm = TRUE),
    combo_lower = quantile(combo_excess, 0.025, na.rm = TRUE),
    combo_upper = quantile(combo_excess, 0.975, na.rm = TRUE),
    baseline_median = quantile(baseline, 0.5, na.rm = TRUE),
    pop = mean(pop, na.rm = TRUE)
  ), by = .(model, age, season)]
  
  ensemble <- bootstrap[
    , .(
      flu_median = mean(flu_median),
      flu_lower = mean(flu_lower),
      flu_upper = mean(flu_upper),
      rsv_median = mean(rsv_median),
      rsv_lower = mean(rsv_lower),
      rsv_upper = mean(rsv_upper),
      hmpv_median = mean(hmpv_median),
      hmpv_lower = mean(hmpv_lower),
      hmpv_upper = mean(hmpv_upper),
      rhino_median = mean(rhino_median),
      rhino_lower = mean(rhino_lower),
      rhino_upper = mean(rhino_upper),
      cov_median = mean(cov_median),
      cov_lower = mean(cov_lower),
      cov_upper = mean(cov_upper),
      combo_median = mean(combo_median),
      combo_lower = mean(combo_lower),
      combo_upper = mean(combo_upper),
      baseline_median = mean(baseline_median),
      pop = mean(pop)
    ),
    by = .(age, season)
  ][
    , model := "Ensemble"
  ]
  
  results = rbind(bootstrap, ensemble)
  return(results)
}

# Function for averaging across seasons  ----------------------------------

average_across_seasons = function(data, pop_col, seasons_to_include){
  
  data <- as.data.table(data)
  pop_col <- as.name(pop_col) 
  
  bootstrap <- data[iteration>0 & season %in% c(seasons_to_include), .(
    flu_excess = sum(flu_ex, na.rm = TRUE),
    rsv_excess = sum(rsv_ex, na.rm = TRUE),
    hmpv_excess = sum(hmpv_ex, na.rm = TRUE),
    rhino_excess = sum(rhino_ex, na.rm = TRUE),
    cov_excess = sum(cov_ex, na.rm = TRUE),
    combo_excess = sum(flu_ex + rsv_ex + hmpv_ex + rhino_ex + cov_ex, na.rm=TRUE),
    pop = mean(eval(pop_col), na.rm = TRUE)
  ), by = .(model, age, iteration, season)][,.(
    flu_average = mean(flu_excess, na.rm=TRUE),
    rsv_average = mean(rsv_excess, na.rm=TRUE),
    hmpv_average = mean(hmpv_excess, na.rm=TRUE),
    rhino_average = mean(rhino_excess, na.rm=TRUE),
    cov_average = mean(cov_excess, na.rm=TRUE),
    combo_average = mean(combo_excess, na.rm=TRUE),
    pop = mean(pop, na.rm=TRUE)
    ), by = .(model, age, iteration)][,.(
    flu_median = quantile(flu_average, 0.50, na.rm = TRUE),
    flu_lower = quantile(flu_average, 0.025, na.rm = TRUE),
    flu_upper = quantile(flu_average, 0.975, na.rm = TRUE),
    rsv_median = quantile(rsv_average, 0.50, na.rm = TRUE),
    rsv_lower = quantile(rsv_average, 0.025, na.rm = TRUE),
    rsv_upper = quantile(rsv_average, 0.975, na.rm = TRUE),
    hmpv_median = quantile(hmpv_average, 0.50, na.rm = TRUE),
    hmpv_lower = quantile(hmpv_average, 0.025, na.rm = TRUE),
    hmpv_upper = quantile(hmpv_average, 0.975, na.rm = TRUE),
    rhino_median = quantile(rhino_average, 0.50, na.rm = TRUE),
    rhino_lower = quantile(rhino_average, 0.025, na.rm = TRUE),
    rhino_upper = quantile(rhino_average, 0.975, na.rm = TRUE),
    cov_median = quantile(cov_average, 0.50, na.rm = TRUE),
    cov_lower = quantile(cov_average, 0.025, na.rm = TRUE),
    cov_upper = quantile(cov_average, 0.975, na.rm = TRUE),
    combo_median = quantile(combo_average, 0.50, na.rm = TRUE),
    combo_lower = quantile(combo_average, 0.025, na.rm = TRUE),
    combo_upper = quantile(combo_average, 0.975, na.rm = TRUE),
    pop = mean(pop, na.rm = TRUE)
  ), by = .(model, age)]
  
  ensemble <- bootstrap[
    , .(
      flu_median = mean(flu_median),
      flu_lower = mean(flu_lower),
      flu_upper = mean(flu_upper),
      rsv_median = mean(rsv_median),
      rsv_lower = mean(rsv_lower),
      rsv_upper = mean(rsv_upper),
      hmpv_median = mean(hmpv_median),
      hmpv_lower = mean(hmpv_lower),
      hmpv_upper = mean(hmpv_upper),
      rhino_median = mean(rhino_median),
      rhino_lower = mean(rhino_lower),
      rhino_upper = mean(rhino_upper),
      cov_median = mean(cov_median),
      cov_lower = mean(cov_lower),
      cov_upper = mean(cov_upper),
      combo_median = mean(combo_median),
      combo_lower = mean(combo_lower),
      combo_upper = mean(combo_upper),
      pop = mean(pop)
    ),
    by = .(age)
  ][
    , model := "Ensemble"
  ]
  
  results = rbind(bootstrap, ensemble)
  return(results)
}

