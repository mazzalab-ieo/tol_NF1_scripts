data_for_composition_plot<-function(data)
{
 #function to transform data to perform a composition plot 
  
  library(tidyverse)
  
  ct<-strsplit(unique(data$Case),"_")[[1]]
  cond<-ct[2]
  
  type<-strsplit(ct[1],"BT")[[1]][2]
  
  #number of unique cells for that condition
  l<-length(unique(data$Name))
  
  d1<-data %>% complete(FRAME) %>%
    mutate(color=factor(color, levels=c("R","G","Y"))) %>%
    group_by(FRAME, color,Case) %>%
    summarize(phase=n(), perc_initial=phase/l*100,
              Condiiton=cond,Type=type)
  
  return(d1)
}