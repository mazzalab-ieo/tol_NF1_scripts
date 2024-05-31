peaks_definition<-function(data,tub,nup,thr)
{
  library(tidyverse)
  library(dplyr)
  library(pracma)
  
  peak_list<-list()
  i<-0
  for (el in unique(data$Case))
  {
    dd1<-data %>%
      filter(Case==el)
    
    for (nel in unique(dd1$ID))
    {
      i<-i+1
      dd2<-dd1 %>% filter(ID==nel)
      e<-unique(dd2$Experiment)
      
      xx<- dd2%>% 
        .$RM_tub
      
      mh_df<-tub %>% filter(Case==el & Experiment==e) 
      
      mh<-mh_df[[thr]]
      
      pk<-as.data.frame(findpeaks(xx,  nups=nup,
                                  minpeakheight =mh
      ))
      
      if(nrow(pk)>0)
      {
        #print(paste(el,nel))
        pk$Case<-el
        pk$ID<-nel
        pk$Experiment<-e
        
        colnames(pk)<-c("Height_peak","Position_peak","Start_peak","End_peak","Case","ID","Experiment")
        
        pk<-pk[order(pk$Start_peak),]
        
        peak_list[[i]]<-pk
      } else
        peak_list[[i]]<-NULL
    }
  }
  
  peaks<-dplyr::bind_rows(peak_list)
  
  return(peaks)
}
