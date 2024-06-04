color_decision_morepoints_2<-function(data)
{
  library(tidyverse)
  # function to assign a color to each timepoint
  
  #compute |Green-Red| to check total difference
  data1 <-data %>% group_by(Name) %>%
    mutate(diff=abs(RollingMean_02-RollingMean_01)) %>%
    ungroup()
  
  #compute derivatives: the frames are consecutive, so no need to compute dx
  data1 <- data %>%
    group_by(Name) %>%
    mutate(dydx_01=c(diff(RollingMean_01),NA), dydx_02=c(diff(RollingMean_02),NA))
  
  #1st step: assign green or red to each timeframe, use the Rolling Mean is better
  data1<-data1 %>% group_by(Name) %>%
    #mutate(color=ifelse(MEAN_INTENSITY01>MEAN_INTENSITY02,"R","G")) %>%
    mutate(color=ifelse(RollingMean_01>RollingMean_02,"R","G")) %>%
    ungroup()
  
    #el<-"2_64"

   for (el in unique(data1$Name))
   {
     print(el)
     
     #start considering the green frames
     tocheck<-data1 %>% filter(Name==el & color=="G") %>% 
   #     #filter(FRAME<meet$Meeting[which(meet$Name==el)]) %>% 
       .$FRAME
     
     #questo prima era nell'if
     der_r1<-data1 %>% filter(Name==el & color=="G") %>%
       filter(dydx_01>1) %>%
       .$FRAME
     
     #### if tocheck is null, then all the points are red and there is no possibility to have a Y phase
     ## also one point with derivative>1 may just be noise
     if (length(tocheck)>0 & length(der_r1)>1)  #prima era if (length(tocheck)>0)
     {
     #in this step is better not to use the frame but the indexes, which are consecutive!
     index<-which(data1[which(data1$Name==el & data1$color=="G"),]$FRAME %in% der_r1)
     
     temp <- cumsum(c(1, diff(index) - 1))
     temp2 <- rle(temp)
     #select group longer than 10 to identify other possible Y phase
     if (length(which(temp2$lengths>=6))>0) #10
     {
     der_r<-der_r1[which(temp %in% with(temp2, values[which(lengths>=10)]))]
     } else
     {
       der_r<-der_r1[which(temp == with(temp2, values[which.max(lengths)]))]
     }
     
     #check: if the last elements of the derivative Y is the last frame-1, 
     # then also the last should be Y. we cannot compute the derivative there!
     if ((length(der_r)>0) & !(is.na(der_r[1]))) #>0
     {
     if (der_r[length(der_r)]== tocheck[length(tocheck)]-1)
     {
       der_r<-c(der_r,tocheck[length(tocheck)])
     }
     }
     
     data1$color[which(data1$Name==el & data1$FRAME %in% c(der_r))]<-"Y"
     
     #check the green part
     ######right side
     # tocheck2<-data1 %>% filter(Name==el & color=="R") %>% 
     #   #filter(FRAME>meet$Meeting[which(meet$Name==el)]) %>% 
     #   .$FRAME
     
     #it should be a very sharp decrease
     der_g<-data1 %>% filter(Name==el & color=="R") %>%
       filter(dydx_02<(-5)) %>% #im not convinced by this threshold
       #filter(dydx_02<0) %>%
       .$FRAME
     
     #but also check less sharp decrease after last Y
     der_g2<-NA
     if(length(data1$FRAME[which(data1$Name==el & data1$color=="Y")]>0))
     {
       der_g2<-data1 %>% filter(Name==el & color=="G") %>%
       filter(dydx_02<(-1) & FRAME>data1$FRAME[which(data1$Name==el & data1$color=="Y")][length(data1$FRAME[which(data1$Name==el & data1$color=="Y")])]) %>% 
       #filter(dydx_02<0) %>%
       .$FRAME
     }
     
     data1$color[which(data1$Name==el & data1$FRAME %in% c(der_g,der_g2))]<-"Y"
     
     ###final fix for those remaining yellow (as BTKO_T2 5_56)
     yellow_ugly<-data1 %>%
       filter(Name==el) %>%
       filter(RollingMean_01>=200 & RollingMean_02>=200) %>%
       .$FRAME
       
     data1$color[which(data1$Name==el & data1$FRAME %in% yellow_ugly)]<-"Y"
     #fix the intermediate color if needed
     #check if there are green parts following the yellow and preceeding the yellow
    
     # index2<-data1$FRAME[which(data1$Name==el & data1$color=="G" & (data1$dydx_02<0 | data1$dydx_01<1))]
     # #index2<-which(data1$Name==el & data1$color=="G" & data1$dydx_01<=1)
     # 
     # for (fr in index2)
     # {
     #   if(fr!=0)
     #   {
     #   i<-which(data1[which(data1$Name==el),]$FRAME==fr)
     #   if (data1$color[which(data1$Name==el)][i-1]=="Y")
     #   {
     #     #print(fr)
     #     data1$color[which(data1$Name==el & data1$FRAME==fr)]<-"Y"
     #   }
     #   }
     # }
     
     ###final check for each point
     for (i in seq(2,length(data1$FRAME[which(data1$Name==el)])-1,by=1))
     {
       if (data1$color[which(data1$Name==el)][i]=="G" & data1$color[which(data1$Name==el)][i-1]=="Y" & data1$color[which(data1$Name==el)][i+1]=="G")
         data1$color[which(data1$Name==el)][i]<-"Y"
       
       if (data1$color[which(data1$Name==el)][i]=="G" & data1$color[which(data1$Name==el)][i-1]=="Y" & data1$color[which(data1$Name==el)][i+1]=="Y")
         data1$color[which(data1$Name==el)][i]<-"Y"
       
       if (data1$color[which(data1$Name==el)][i]=="Y" & data1$color[which(data1$Name==el)][i-1]=="R")
       data1$color[which(data1$Name==el)][i]<-"R"
       
       if (data1$color[which(data1$Name==el)][i]=="G" & data1$color[which(data1$Name==el)][i-1]=="R" & data1$color[which(data1$Name==el)][i+1]=="R")
         data1$color[which(data1$Name==el)][i]<-"R"
     }
     
     #fix the last point if needed
     max_frame<-which(data1$FRAME[which(data1$Name==el)]==max(data1$FRAME[which(data1$Name==el)]))
     if (!(data1$color[which(data1$Name==el)][max_frame]=="Y") & data1$color[which(data1$Name==el)][max_frame-1]=="Y")
       data1$color[which(data1$Name==el)][max_frame]<-"Y"
     }
     else
     {
       ###final fix for those remaining yellow (as BTKO_T2 5_56) also in the case of everything red
     yellow_ugly<-data1 %>%
       filter(Name==el) %>%
       filter(RollingMean_01>=200 & RollingMean_02>200) %>%
       .$FRAME
     
     data1$color[which(data1$Name==el & data1$FRAME %in% yellow_ugly)]<-"Y"
     
     #check 
     for (i in seq(2,length(data1$FRAME[which(data1$Name==el)])-1,by=1))
     {
     if (data1$color[which(data1$Name==el)][i]=="Y" & data1$color[which(data1$Name==el)][i-1]=="R")
       data1$color[which(data1$Name==el)][i]<-"R"
     }
     
     }
   }
     
  return(data1)
}


