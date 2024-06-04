plot_cell_story<-function(data1,name, case)
{
  #plot the complete cell cycle for all the cells in the selected condition
  #name: selected condition
  
  library(tidyverse)
  library(wesanderson)
  
  data2<- data1 %>% complete(FRAME)

  if (case=="death")
  {

    mycol<-c("R"="brown1","G"="darkgreen","Y"="darkgoldenrod1","Lost"="darkgrey","Dead"="black")
    
    ordered_names<-data2 %>% 
      group_by(Name) %>%
      summarize(First_col=rle(color_death)$values[1],First_length=rle(color_death)$lengths[1]) %>%
      mutate(First_col=factor(First_col,levels=c("R","G","Y","Lost","Dead")))%>%
      arrange(First_col,desc(First_length)) %>%
      ungroup()%>%
      .$Name
    
    pl<-data2 %>% 
      arrange(factor(Name, levels = rev(ordered_names))) %>%
      group_by(Name) %>%
      ggplot()+geom_tile(aes(x=FRAME,y=factor(Name, levels=unique(Name)),fill=color_death))+
      scale_fill_manual(values=c(mycol))+
      guides(fill=FALSE)+
      scale_x_continuous(limits=c(0,531),breaks=seq(0,530,by=50))+ 
      theme_classic(base_size=15)+
      theme(axis.text.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank())+
      labs(y="",x="FRAME", title=name)
  }
    else
    {
      if (case=="lost")
      {
        #print(table(data2$color_death))
        
        data3<- data2 %>%
          mutate(color_final=ifelse(color_death %in% c("Dead","Lost"),"Lost",color_death))
        
        #print(table(data3$color_final))
        
        mycol<-c("R"="brown1","G"="darkgreen","Y"="darkgoldenrod1","Lost"="black")
        
        ordered_names<-data3 %>%
          group_by(Name) %>%
          summarize(First_col=rle(color_final)$values[1],First_length=rle(color_final)$lengths[1]) %>%
          mutate(First_col=factor(First_col,levels=c("R","G","Y","Lost")))%>%
          arrange(First_col,desc(First_length)) %>%
          ungroup()%>%
          .$Name
        
        mf<-max(data3$FRAME)
        pl<-data3 %>% 
          arrange(factor(Name, levels = rev(ordered_names))) %>%
          group_by(Name) %>%
          ggplot()+geom_tile(aes(x=FRAME,y=factor(Name, levels=unique(Name)),fill=color_final))+
          scale_fill_manual(values=c(mycol))+
          guides(fill=FALSE)+
          scale_x_continuous(limits=c(0,mf+1),breaks=seq(0,mf,by=50),expand=c(0,0))+
          theme_classic(base_size=15)+
          theme(panel.background = element_rect(fill = 'black'), axis.text.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank())+
          labs(y="",x="FRAME", title=name)
      }
      else
    {
  mycol<-c("R"="red","G"="green","Y"="yellow")
      
  ordered_names<-data2 %>% 
    group_by(Name) %>%
    summarize(First_col=rle(color)$values[1],First_length=rle(color)$lengths[1]) %>%
    mutate(First_col=factor(First_col,levels=c("R","G","Y")))%>%
    arrange(First_col,desc(First_length)) %>%
    ungroup()%>%
    .$Name
  
  pl<-data2 %>% 
      arrange(factor(Name, levels = rev(ordered_names))) %>%
      group_by(Name) %>%
    ggplot()+geom_tile(aes(x=FRAME,y=factor(Name, levels=unique(Name)),fill=color))+
    scale_fill_manual(values=c(mycol))+
    guides(fill=FALSE)+
    scale_x_continuous(limits=c(0,531),breaks=seq(0,530,by=50))+
    theme_classic(base_size=15)+
    theme(panel.background = element_rect(fill = 'black'), axis.text.y=element_blank(),axis.line.y=element_blank(),axis.ticks.y=element_blank())+
    labs(y="",x="FRAME", title=name)
    }
    }
  
  return(pl)
}