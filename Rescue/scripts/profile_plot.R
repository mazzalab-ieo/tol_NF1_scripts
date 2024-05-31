profile_plot<-function(data, choose_el,choose_name,choose_lev,tub_bycondition, dcn_fin_holes,save)
{
  library(tidyverse)
  library(RColorBrewer)

#order: Hole, Microtubule,Tail
mycol1_bis<-c("orchid2","darkmagenta","#969696") 

mycol2<-c(brewer.pal(10,"Paired")[c(3,4)],brewer.pal(9,"Set1")[9]) 

dd<-data_tub_final %>%
  filter(Case==choose_el)

dd$Internal_peak<-factor(dd$Internal_peak,levels=c("No","Yes","Tail"))

for (nel in unique(choose_name))
{
  dd1<-dd %>% filter(ID==nel)
  e<-unique(dd1$Experiment)
  
  if (choose_lev>0)
  {
    dd1$lev<-choose_lev
  }
  else
  {
    dd1$lev<-case_when(e==1 & el=="NF1_DM1" ~40,
                     e==1 & el=="NF1"~7,
                     e==1 & el %in% c("Control","DM1")~10,
                     e==2 & el %in% c("Control","NF1","NF1_DM1") ~ 30,
                     e==2 & el=="DM1"~10)
  }
  
  pl1<- dd1%>%
    ggplot()+
    geom_vline(aes(xintercept=start_c),linetype=2,color=mycol1_bis[3])+
    geom_vline(aes(xintercept=end_c),linetype=2,color=mycol1_bis[3])+
    
    geom_line(aes(x=Microns,y=RM_Norm,color="Microtubule"),linewidth=1)+
    geom_line(aes(x=Microns,y=adap_70,color="Damage threshold"),linetype=2,linewidth=0.8)+
    
    geom_line(aes(x=Microns,y= RM_tub/lev,color="Tubulin"),linewidth=1)+

    geom_hline(aes(yintercept=tub_bycondition %>% filter(Case==el & Experiment==e) %>% .$q50_RMtub_condition/lev,
                   color="Peaks threshold"),
               linetype=2,
               linewidth=0.8)+
    scale_color_manual(values=c(mycol1_bis[2],mycol1_bis[1],mycol2[2],mycol2[1]))+
    scale_y_continuous(
      # Features of the first axis
      name = "Normalize Intensity microtubule (RM)",
      # Add a second axis and specify its features
      sec.axis = sec_axis( trans=~.*dd1$lev[1], name="Tubulin intensity (RM)")
    ) +
    labs(x="Microns",title=paste(el,nel, sep=":"),color="")+
    theme_classic(base_size=20)+
    theme(plot.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(color = mycol1_bis[2], size=15),
          axis.title.y.right = element_text(color = mycol2[2], size=15),
          legend.position ="top"
    )
  
  pl2<-dd1 %>%
    filter(ID==nel) %>%
    ggplot()+
    geom_tile(aes(x=Microns, y=0,fill=Hole_general),alpha=0.7)+
    scale_fill_manual(values=mycol1_bis,labels=c("Hole","Microtubule","Tail"),drop = F)+
    geom_text(data=dcn_fin_holes %>%
                filter(Case==choose_el & ID==nel & values==0 & Hole_05=="Yes"),
              aes(x=start_M+0.1,y=0),label="*",size=8)+
    labs(y="",x="Microns",fill="Microtubule")+
    theme_void(base_size = 15)+
    theme(legend.position = "bottom")
  
  leg2<-get_legend(pl2)
  
  pl2<-pl2+guides(fill=FALSE)
  
  #tubulin peaks 
  pl4<-dd1 %>%
    ggplot()+
    geom_tile(aes(x=Microns, y=0,fill=Internal_peak),alpha=0.7)+
    scale_fill_manual(values=mycol2,labels=c("Normal","Peak","Tail"),drop = F)+
    labs(y="",x="Microns",fill="Tubulin")+
    theme_void(base_size=15)+
    theme(legend.position = "bottom")
  
  leg4<-get_legend(pl4)
  pl4<-pl4+guides(fill=FALSE)
  
  #to plot repaired holes you can add this part 
  #order is "Tail","Microtubule","Unrepaired","Repaired"
  # pl5<-dd1 %>%
  #   ggplot()+
  #   geom_tile(aes(x=Microns, y=0,fill=Filled))+
  #   scale_fill_manual(values=c(rev(mycol1),mycol2[2]),
  #                     labels=c("Tail","Microtubule","Unrepaired\nhole (>0.5 microns)","Repaired\nhole (>0.5 microns)"),
  #                     drop = F)+
  #   labs(y="",x="Microns",fill="Repair")+
  #   theme_void()
  # 
  # leg5<-get_legend(pl5)
  # 
  # pl5<-pl5+guides(fill=FALSE)
  
  pleg<-plot_grid(leg2,leg4,ncol=2)
  
  pl6<-plot_grid(pl1, pl2,
                 pl4, #pl5,
                 pleg,
                 rel_heights=c(0.85, 0.05,0.05,0.05), 
                 ncol = 1, align = "v")
  
  if (save==TRUE)
  {
    save_plot(path = "/results",filename=paste(el, "_", nel,".pdf",sep=""),  pl6, ncol = 1, nrow=2,base_asp=3)
  }
  else
  {
    print(pl6)
  }
}

}