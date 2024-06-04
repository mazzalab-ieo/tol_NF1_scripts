is_dead<-function(data)
{
  # function to decide whether a lost cell (defined by a "Grey" label in data) 
  # is dead or just lost by the tracker
  
  # data have already passed from the color_lost_assign function
  # you also need a Live/Dead channel, named as MeanIntensity04
  
  library(tidyverse)
  
  for (el in unique(data$Name))
  {
    unicol<-unique(data$color_death[which(data$Name==el)])
    if ("Grey" %in% unicol)
    {
      maxfr<- which(data$FRAME[which(data$Name==el)]==max(data$FRAME[which(data$Name==el)]))
      
     #check the Live/Dead channel, is described in MeanIntensity04
      mingrey<-data$FRAME[which(data$Name==el & data$color_death=="Grey")[1]]
      
      if (max(data$MEAN_INTENSITY04[which(data$Name==el & data$FRAME>=mingrey-10)])>=30)
        {
          data$color_death[which(data$Name==el & data$color_death=="Grey")]<-"Dead"
        }
      else
      {
        data$color_death[which(data$Name==el & data$color_death=="Grey")]<-"Lost"
      }
      
    }
  }
  return(data)
}