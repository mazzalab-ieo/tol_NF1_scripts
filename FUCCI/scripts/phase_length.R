phase_length<-function(data,case)
{
  #compute the number of frames assigned to each color and the total length of the cell cycle
  library(tidyverse)
  
  if (case=="death")
  {
    l<-data %>% group_by(Case,Name,color_death) %>%
    summarize(length=n()) 
  }
  else 
  {
    l<-data %>% group_by(Case,Name,color) %>%
      summarize(length=n()) 
  }
  
  l1<-data %>%
    group_by(Case, Name) %>%
    summarize(total_length=n())

  lfin<-left_join(l,l1,by=c("Case","Name"))
  
  return(lfin)
  
}