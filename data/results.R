

  library("tidyverse")
  library("here")

  data_wiesner <- readRDS(here::here("data", "results.rds")) %>% 
    arrange(Headloss) %>% 
    select(p1, p2, Headloss, Flow)
  
  mean_value <- data_wiesner %>% 
    filter(Headloss <= mean(data_wiesner$Headloss)) %>% 
    arrange(desc(Headloss), p1) %>% 
    head(1)
  
  headloss  <- c( min  = min(data_wiesner$Headloss), 
                  max  = max(data_wiesner$Headloss))
  
  data_wiesner <- data_wiesner %>% 
    filter(Headloss %in% headloss) 
  