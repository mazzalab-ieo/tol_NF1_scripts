holes_length<-function(data,bound,lower_lt,lt)
{
  #bound: threshold to identify a hole
  #lower_lt:minimum length to consider and draw a hole
  #lt:length to call it a PROPER HOLE
  
  library(tidyverse)
  
# find the length of the holes
  dcn_rle<-list()
  i<-0
  for (case in unique(data$Case))
  {
    dc<-data %>% 
      filter(Case==case)
    
    for (n in unique(dc$Name))
    {
      i<-i+1
      # print(i)
      dcn<-dc %>% filter(Name==n)
      
      aa<-rle(dcn[[bound]]) %>% 
        unclass() %>%
        as.data.frame() %>%
        mutate(end = cumsum(lengths),
               start = c(1, dplyr::lag(end)[-1] + 1)) %>%
        magrittr::extract(c(1,2,4,3)) %>%
        mutate(Case=case,Name=n)
      
      aa$start_M<-dcn$Microns[aa$start]
      aa$end_M<-dcn$Microns[aa$end]
      
      dcn_rle[[i]]<-aa
    }
  }
  dcn_fin <- dplyr::bind_rows(dcn_rle)
  dcn_fin_holes <- dcn_fin %>% #filter(values==0) %>% 
    mutate(ltot=end_M-start_M,
           Hole_general=ifelse(values==0 & lengths>1 & ltot>lower_lt,"Yes","No"),
           Hole_05=ifelse((values==0 & ltot>=lt),"Yes","No"))
  
  ### define if a hole is larger than 0.5 microtubules or not
  c1<-character()
  c2<-character()
  c3<-character()
  c4<-character()
  for (i in seq(1,nrow(dcn_fin_holes)))
  {
    c1<-c(c1,rep(dcn_fin_holes$Case[i],dcn_fin_holes$lengths[i]))
    c2<-c(c2,rep(dcn_fin_holes$Name[i],dcn_fin_holes$lengths[i]))
    c3<-c(c3,rep(dcn_fin_holes$Hole_05[i],dcn_fin_holes$lengths[i]))
    c4<-c(c4,rep(dcn_fin_holes$Hole_general[i],dcn_fin_holes$lengths[i]))
  }
  
  dcn_final<-data.frame(Case=c1,Name=c2,Hole_general=c4,Hole_05=c3)
  
  dcn_final<-dcn_final%>% 
    group_by(Case, Name) %>%
    mutate(position=1:n())
  
  return(list(dcn_fin_holes,dcn_final))
}