rolling_mean<-function(data,window)
{
  #calculate rolling mean with the selected window
  #window: number of points before/after the considered one
  # ex: if window=2, the mean is computed on 5 points, (i-2,i-1,i,i+1,i+2)
  
  library(tidyverse)
  library(slider)
  
  data1 <-data %>% 
    group_by(Name) %>%
    mutate("RollingMean_01"=slide_dbl(MEAN_INTENSITY01, mean, .before = window, .after = window),
           "RollingMean_02"=slide_dbl(MEAN_INTENSITY02, mean, .before = window, .after = window)) %>%
    ungroup()
  
  return(data1)
}