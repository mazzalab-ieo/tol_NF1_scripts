peaks_bymicron<-function(data,peaks,tub,thr)
{
  library(tidyverse)
  
  c1<-character()
  c2<-character()
  cpp<-numeric()
  pos<-numeric()
  
  for (el in unique(data$Case))
  {
    #print(el)
    dt<-data %>% filter(Case==el)
    dp<-peaks %>% filter(Case==el)
    
    for(nel in unique(dt$ID))
    {
      tryp<-dt %>% filter(ID==nel)
      tryp2<-dp%>% filter(ID==nel)
      nr<-nrow(tryp)
      
      if (nrow(tryp2)>0)
      {
        c1<-c(c1,rep(el,nr))
        c2<-c(c2,rep(nel,nr))
        cp<-rep(0,nr)
        pos<-c(pos,seq(1,nr))
        
        e<-unique(tryp2$Experiment)
        
        for (i in 1:nrow(tryp2))
        {
          pp<-tryp2$Position_peak[i]
          if (tryp$Microns[pp]>=tryp$start_c[pp] & tryp$Microns[pp]<=tryp$end_c[pp])
          {
            #print(i)
            peaks_positions<-seq(tryp2$Start_peak[i],tryp2$End_peak[i],by=1)
            
            tb50_df<-tub %>% filter(Case==el & Experiment==e) 
            
            tb50<-tb50_df[[thr]]
            
            #define as a hole only where the RM goes above the percentile defined by the threshold
            morethan50<-peaks_positions[which(tryp$RM_tub[peaks_positions]>=tb50)]
            
            cp[morethan50]<-1
          }
        }
        cpp<-c(cpp,cp)
      }
    }
  }
  
  peaks_bymic<-data.frame(Case=c1,ID=c2,position=pos,Is_peak=cpp)
  
  return(peaks_bymic)
}