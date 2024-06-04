color_lost_assign<-function(data)
{
  # function to assign a "grey" label (lost) as a cell phase
  # data: has already a first evaluation of cell phase
  
  library(tidyverse)
  library(zoo)
  
  #define ratio of channels that should remain constant for the dying cells
  data1<- data %>% group_by(Name) %>%
    mutate(ratio=RollingMean_01/RollingMean_02)
  
  #to define the ratio constant, we use the Coefficient of Variation, that should stay small on a window
  data2<-data1 %>% 
    group_by(Name) %>% 
    mutate(RollMeanCV=rollapplyr(ratio,7,function(x) sd(x)/mean(x),fill=NA))
  
   data2<-data2 %>%
     group_by(Name) %>% 
     mutate(perc_R=RollingMean_01/(RollingMean_01+RollingMean_02)*100) %>%
     mutate(RollMean_der01=rollapplyr(dydx_01,20,mean,fill=NA)) %>%
     mutate(RollMean_der02=rollapplyr(dydx_02,10,mean,fill=NA))
   
  data2<- data2 %>%
    mutate(color_death=color)
  
  names<-unique(data2$Name)
  for (el in names)
  {
    print(el)
    
    data3<-data2 %>% filter(Name==el)
    
    data3_nochange<-data2 %>% filter(!(Name==el))
    
    maxframeCV<-max(data3$FRAME[which(data3$RollMeanCV>0.1)])
    
    if(!(is.infinite(maxframeCV)))
    {
      index<-which(data3$FRAME>maxframeCV & 
                     data3$perc_R<80 & 
                     data3$RollMean_der01<0.5)
      
      data3$color_death[index]<-"Grey"
    }
    
    # check the cases where Red channel is super high: it can't be dying
    ind_check<-which(data3$color_death=="Grey" & data3$RollingMean_01>=200)
    data3$color_death[ind_check]<-data3$color[ind_check]
    
    # check last point, since the last is NA, use number of rows
    max_frame<-nrow(data3)
    
    if (!(data3$color_death[max_frame]=="Grey") & 
        (data3$color_death[max_frame-1]=="Grey"))
    {
      data3$color_death[max_frame]<-"Grey"
    }
    ### fix the trend red-green at the end of the timelapse
    col_all2<-data3$color_death

    temp <- rle(col_all2)
    if (length(temp$lengths)>=3) #otherwise there are not even 3 phases assigned
    {
      if (temp$values[length(temp$lengths)]=="Grey" & 
          temp$values[length(temp$lengths)-1]=="G" & 
          temp$values[length(temp$lengths)-2]=="R")
      {
        ind_start1<-sum(temp$lengths[seq(1,length(temp$lengths)-1,1)])+1
        for (i in seq(ind_start1,max_frame-1,1))
        {
          if (data3$RollMean_der02[i]>-0.1)
          {
            data3$color_death[i]<-data3$color[i]
          }
        }
        if ((data3$color_death[max_frame]=="Grey") & 
            data3$color_death[max_frame-1]=="G")
          
          data3$color_death[max_frame]<-"G"
        
      }
    }
  
    #check if there are grey intermediate
    if (("Grey" %in% unique(data3$color_death)) & !(data3$color_death[max_frame]=="Grey"))
    {
      fix1<-which(data3$color_death=="Grey")
      data3$color_death[fix1]<-data3$color[fix1]
    }
    
    #check the holes between the remaining grey
    if ((data3$color_death[max_frame]=="Grey") & 
        #("Grey" %in% unique(data3$color_death[data3$FRAME<max_frame])) ###FIX 
        length(which(data3$color_death=="Grey")>1)) 
    {
      index_grey<-which(data3$color_death=="Grey")
      frame_grey<-data3$FRAME[index_grey]
      
      data3$color_death[which(data3$FRAME<=max(frame_grey) & data3$FRAME>=min(frame_grey))]<-"Grey"
    }
    
    #fix when Grey starts. usually we say red even if it's yellow or already dead.
    col_all<-data3$color_death

    temp2 <- rle(col_all)
    if (length(temp2$lengths)>=3)
    {
    if (temp2$values[length(temp2$lengths)]=="Grey" & 
        temp2$values[length(temp2$lengths)-1]=="R" & 
        temp2$values[length(temp2$lengths)-2]=="Y")
    {
      if (temp2$lengths[length(temp2$lengths)-1]<50)
      {
        ind_start<-sum(temp2$lengths[seq(1,length(temp2$lengths)-2,1)])+1
        data3$color_death[seq(ind_start,max_frame,1)]<-"Grey"
      }
    }
    }
    
    data2<-rbind(data3,data3_nochange)
  }

  return(data2)
}