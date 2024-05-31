tips<-function(dd,var)#ata,el,nel,exp)
{
  library(tidyverse)
  library(pracma)
  
  x<-dd[[var]]
  
  fp<-as.data.frame(findpeaks(x,  nups=3))
  
  colnames(fp)<-c("Height_peak","Position_peak","Start_peak","End_peak")
  
  fp<-fp[order(fp$Position_peak),]
  
  fp1<-as.data.frame(findpeaks(x,  nups=0, ndowns=3))
  colnames(fp1)<-c("Height_peak","Position_peak","Start_peak","End_peak")
  
  fp1<-fp1[order(fp1$Position_peak),]
  
  fp2<-as.data.frame(findpeaks(x,  nups=3, ndowns=0))
  colnames(fp2)<-c("Height_peak","Position_peak","Start_peak","End_peak")
  
  fp2<-fp2[order(fp2$Position_peak),]
  
  ####
  start<-ifelse(nrow(fp1)>0,fp1$End_peak[1],fp$End_peak[1])
  end<-ifelse(nrow(fp2)>0,fp2$Start_peak[nrow(fp2)],fp$Start_peak[nrow(fp)])
  
  #start<-fp$End_peak[1]
  #end<-fp$Start_peak[nrow(fp)]
  
  t_comp<-data.frame(start=start,end=end)
  return(t_comp)
}