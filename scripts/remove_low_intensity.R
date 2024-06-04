remove_low_intensity<-function(data,thr)
{
  #remove curves with both G and R channel lower than threshold
  library(tidyverse)
  
  max_sum<-data %>% group_by(Name) %>%
    summarize(max1=max(MEAN_INTENSITY01),max2=max(MEAN_INTENSITY02)) %>%
    mutate(Remove=ifelse((max1<thr & max2<thr),"Yes","No"))
           
  keep<-max_sum$Name[which(max_sum$Remove=="No")]
  
data1<-data %>%
  filter(Name %in% keep)

return(data1)
}