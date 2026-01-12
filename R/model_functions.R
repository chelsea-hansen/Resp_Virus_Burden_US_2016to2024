
# Function for calculating excess -----------------------------------------


model_results = function(model_name, mod_dat, base_dat, flu_dat, rsv_dat, hmpv_dat, rhino_dat, cov_dat){
  pred = predict(model_name, newdata=mod_dat, type="response")
  baseline = predict(model_name, newdata=base_dat, type="response")
  flu_ex = pred - predict(model_name, newdata=flu_dat, type="response")
  rsv_ex = pred - predict(model_name, newdata=rsv_dat, type="response")
  hmpv_ex = pred - predict(model_name, newdata=hmpv_dat, type="response")
  rhino_ex = pred - predict(model_name, newdata=rhino_dat, type="response")
  cov_ex = pred - predict(model_name, newdata=cov_dat, type="response")
  results = data.frame(date = mod_dat$date, 
                       season = mod_dat$season,
                       pop_total = mod_dat$pop, 
                       pop_enrolled = mod_dat$pop_enrolled,
                       obs=model_name$y, 
                       pred=pred, 
                       baseline=baseline, 
                       flu_ex=flu_ex, 
                       rsv_ex=rsv_ex, 
                       hmpv_ex=hmpv_ex,
                       rhino_ex=rhino_ex,
                       cov_ex=cov_ex)
  return(results)
}


# Function for running Models ------------------------------------



bootstrap_block_residual <- function(dat, 
                                     y_value, 
                                     ages, 
                                     lags,
                                     include_viruses_A,
                                     include_viruses_B,
                                     include_viruses_C,
                                     include_viruses_D,
                                     include_viruses_E,
                                     block_length, 
                                     n_boot = 100) {
  
  model_names <- c("Model_A","Model_B","Model_C","Model_D","Model_E")
  all_boot_results <- list()
  
  
  for (model_name in model_names) {
    cat("Bootstrapping", model_name, "...\n")
    
    for (age_group in ages) {
      mod_dat <- dat %>% filter(agegrp == age_group) %>% arrange(year_decimal)
      
      if (age_group =="<1") {
        best_lag <- lags[1]
      } else if (age_group == "1to4"){
        best_lag <- lags[2]
      }else if (age_group=="5to49"){
        best_lag = lags[3]
      }else if (age_group=="50to64"){
        best_lag = lags[4]
      }else if (age_group=="65+"){
        best_lag = lags[5]
      }
      
      # Apply lag if needed
      mod_dat <- mod_dat %>%
        mutate(
          flu_proxy = lag(flu_proxy, best_lag),
          fluAh3n2_proxy = lag(fluAh3n2_proxy, best_lag),
          fluAh1n1pdm09_proxy = lag(fluAh1n1pdm09_proxy, best_lag),
          fluB_proxy = lag(fluB_proxy, best_lag),
          rsv_proxy = lag(rsv_proxy, best_lag),
          rsv_proxy_alternative = lag(rsv_proxy_alternative, best_lag),
          hmpv_proxy = lag(hmpv_proxy, best_lag),
          rhino_proxy = lag(rhino_proxy, best_lag),
          covid_proxy = lag(covid_proxy, best_lag),
        ) %>%
        slice((max(lags)+1):n())
      
      mod_dat$mod_y = mod_dat[[y_value]]
      
      degrees = round((nrow(mod_dat)/52.18)*3)+1
      virus_listA <- paste0("+", paste(include_viruses_A, collapse = "+"))
      virus_listB <- paste0("+", paste(include_viruses_B, collapse = "+"))
      virus_listC <- paste0("+", paste(include_viruses_C, collapse = "+"))
      virus_listD <- paste0("+", paste(include_viruses_D, collapse = "+"))
      virus_listE <- paste0("+", paste(include_viruses_E, collapse = "+"))
      
      form_model_A <- as.formula(
        paste0("mod_y ~ poly(year_decimal, 2) + sin1 + cos1 + flu_proxy",virus_listA))
      
      form_model_B <- as.formula(
        paste0("mod_y ~ ns(year_decimal, df = 2) + s(mmwr_week, k = 12, bs = 'cc') + fluAh3n2_proxy + fluAh1n1pdm09_proxy + fluB_proxy",virus_listB))
      
      form_model_C <- as.formula(
        paste0("mod_y ~ ns(year_decimal, df = 2) + s(mmwr_week, k = 12, bs = 'cc') + flu_proxy:season",virus_listC))
      
      form_model_D <- as.formula(
        paste0("mod_y ~ ns(year_decimal, df=", degrees, ") + fluAh3n2_proxy + fluAh1n1pdm09_proxy + fluB_proxy",virus_listD))
      
      form_model_E <- as.formula(
        paste0("mod_y ~ ns(year_decimal, df=", degrees, ") + flu_proxy:season", virus_listE))   
      
      
      # Define datasets for result extraction (unchanged)
      base_dat <- mod_dat %>% mutate(flu_proxy = 0, fluAh3n2_proxy = 0, fluAh1n1pdm09_proxy = 0, fluB_proxy = 0,
                                     rsv_proxy = 0, rsv_proxy_alternative = 0, hmpv_proxy = 0,rhino_proxy=0,covid_proxy=0)
      flu_dat <- mod_dat %>% mutate(flu_proxy = 0, fluAh3n2_proxy = 0, fluAh1n1pdm09_proxy = 0, fluB_proxy = 0)
      rsv_dat <- mod_dat %>% mutate(rsv_proxy=0, rsv_proxy_alternative=0)
      hmpv_dat <- mod_dat %>% mutate(hmpv_proxy = 0)
      rhino_dat <- mod_dat %>% mutate(rhino_proxy = 0)
      cov_dat <- mod_dat %>% mutate(covid_proxy = 0)
      
      
      
      
      # Fit original model
      mod_fit <- switch(model_name,
                        "Model_A" = gam(form_model_A, data = mod_dat, method = "REML", family = gaussian()),
                        "Model_B" = gam(form_model_B, data = mod_dat, method = "REML", family = gaussian()),
                        "Model_C" = gam(form_model_C, data = mod_dat, method = "REML", family = gaussian()),
                        "Model_D" = gam(form_model_D, data = mod_dat, method = "REML", family = gaussian()),
                        "Model_E" = gam(form_model_E, data = mod_dat, method = "REML", family = gaussian())
                        
      )
    
      
      # Save original model result (iteration = 0)
      original_result <- model_results(mod_fit, mod_dat, base_dat, flu_dat, rsv_dat, hmpv_dat, rhino_dat, cov_dat) %>%
        mutate(model = model_name, age = age_group, iteration = 0)
      all_results <- list(original_result)
      
      
      #Block bootstrap of residuals
      
      resids <- residuals(mod_fit)
      n_obs <- nrow(mod_dat)
      
      
      # Bootstrap loop: parametric NB (no blocks)
      for (i in 1:n_boot) {
        
        # replace response with simulated counts
        n_blocks <- ceiling(n_obs / block_length)
        block_starts <- sample(1:(n_obs - block_length + 1), n_blocks, replace = TRUE)
        boot_resids <- unlist(lapply(block_starts, function(start) resids[start:(start + block_length - 1)]))
        boot_resids <- boot_resids[1:n_obs]  # trim to exact length
        
        # Create bootstrap response
        mod_dat_boot <- mod_dat
        mod_dat_boot$mod_y <- fitted(mod_fit) + boot_resids
        
        
        # refit model on bootstrapped data (same formula)
        boot_fit <- switch(model_name,
                           "Model_A" = gam(form_model_A, data = mod_dat_boot, method = "REML", family = gaussian()),
                           "Model_B" = gam(form_model_B, data = mod_dat_boot, method = "REML", family = gaussian()),
                           "Model_C" = gam(form_model_C, data = mod_dat_boot, method = "REML", family = gaussian()),
                           "Model_D" = gam(form_model_D, data = mod_dat_boot, method = "REML", family = gaussian()),
                           "Model_E" = gam(form_model_E, data = mod_dat_boot, method = "REML", family = gaussian())
        )
        
        # collect bootstrap result
        boot_result <- model_results(boot_fit, mod_dat_boot, base_dat, flu_dat, rsv_dat, hmpv_dat, rhino_dat, cov_dat) %>%
          mutate(model = model_name, age = age_group, iteration = i)
        all_results[[i + 1]] <- boot_result
      }
      
      all_boot_results[[paste0(model_name, "_", age_group)]] <- bind_rows(all_results)
    }
  }
  
  return(bind_rows(all_boot_results))
}


