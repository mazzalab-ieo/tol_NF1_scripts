print_images_double<-function(data,print,save)
{
  #print the cell cycle intensity and story for a specific cell
  
  library(tidyverse)
  library(cowplot)
  
  mycol<-c("R"="brown1","G"="darkgreen","Y"="darkgoldenrod1","Lost"="grey","Dead"="black")
  
  for (el in unique(data$Name))
  {
  pl1<- data %>% 
    filter(Name==el) %>%
    ggplot()+
    geom_line(aes(x=FRAME,y=RollingMean_01),col="brown1",size=1.3)+
    geom_line(aes(x=FRAME,y=RollingMean_02),col="darkgreen",size=1.3)+
    labs(y="Intensity",x="",title=paste(data$Case[1], ", FOV=",strsplit(el,"_")[[1]][1], ", ID=",strsplit(el,"_")[[1]][2],sep=""))+
    theme_classic(base_size=20)
  
  # pl2<-data %>% 
  #   filter(Name==el) %>%
  #   ggplot()+
  #   geom_tile(aes(x=FRAME, y=0,fill=color))+
  #   scale_fill_manual(values=mycol)+
  #   guides(fill=FALSE)+
  #   labs(y="",x="FRAME")+
  #   theme_void()
  
  pl3<-data %>%
    filter(Name==el) %>%
    ggplot()+
    geom_tile(aes(x=FRAME, y=0,fill=color_death))+
    scale_fill_manual(values=mycol)+#,labels=c("S","G1","G2/M","Lost","Dead"),drop = F)+
    labs(y="",x="FRAME",fill="")+
    theme_void(base_size=20)+
    theme(legend.position = "bottom")
  
  leg3<-get_legend(pl3)
  pl3<-pl3+guides(fill=FALSE)
  
  #pleg<-plot_grid(NULL,leg3,ncol=1)
  
  pltot<-plot_grid(pl1,#pl2,
                   pl3,
                   #pleg,
                   rel_heights=c(0.95, 0.05),#0.05),
                   ncol = 1, align = "v")
  #print(pl3)
  #dev.off()
  
  if (print==TRUE)
    print(pltot)
  
  if (save==TRUE)
    save_plot(path="/results",filename=paste(data$Case[1], "_", el,".pdf",sep=""), pltot, ncol = 1, nrow=3, base_height=1.5,base_asp = 3)
  }
  
}