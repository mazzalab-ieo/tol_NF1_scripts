### analysis on complete dataset

library(tidyverse)
library(RColorBrewer)
library(ggridges)
library(patchwork)
library(car)
library(ggseqplot)
library(wesanderson)
library(introdataviz)
library(rstatix)

setwd("~/FUCCI")

source('scripts/remove_low_intensity.R')
source('scripts/rolling_mean.R')
source('scripts/color_decision_morepoints_2.R')
source('scripts/color_lost_assign.R')
source('scripts/is_dead.R')
source('scripts/print_images_double.R')
source('scripts/plot_cell_story.R')
source('scripts/data_for_composition_plot.R')
source('scripts/phase_length.R')

##### read files for KO
names<-c("CTRL","T1","T2")

for (el in names)
{
  print(el)
  d1 <- read.delim(paste("data/BTKO/Spots_BTKO_",el,"_00.txt",sep=""), stringsAsFactors=FALSE)
  d1$Fov<-0
  d1$Name<-paste(d1$Fov,d1$TRACK_ID,sep="_")
  d2 <- read.delim(paste("data/BTKO/Spots_BTKO_",el,"_01.txt",sep=""), stringsAsFactors=FALSE)
  d2$Fov<-1
  d2$Name<-paste(d2$Fov,d2$TRACK_ID,sep="_")
  d3 <- read.delim(paste("data/BTKO/Spots_BTKO_",el,"_02.txt",sep=""), stringsAsFactors=FALSE)
  d3$Fov<-2
  d3$Name<-paste(d3$Fov,d3$TRACK_ID,sep="_")
  d4 <- read.delim(paste("data/BTKO/Spots_BTKO_",el,"_03.txt",sep=""), stringsAsFactors=FALSE)
  d4$Fov<-3
  d4$Name<-paste(d4$Fov,d4$TRACK_ID,sep="_")
  d5<- read.delim(paste("data/BTKO/Spots_BTKO_",el,"_04.txt",sep=""), stringsAsFactors=FALSE)
  d5$Fov<-4
  d5$Name<-paste(d5$Fov,d5$TRACK_ID,sep="_")
  d6 <- read.delim(paste("data/BTKO/Spots_BTKO_",el,"_05.txt",sep=""), stringsAsFactors=FALSE)
  d6$Fov<-5
  d6$Name<-paste(d6$Fov,d6$TRACK_ID,sep="_")
  d7 <- read.delim(paste("data/BTKO/Spots_BTKO_",el,"_06.txt",sep=""), stringsAsFactors=FALSE)
  d7$Fov<-6
  d7$Name<-paste(d7$Fov,d7$TRACK_ID,sep="_")
  d8 <- read.delim(paste("data/BTKO/Spots_BTKO_",el,"_07.txt",sep=""), stringsAsFactors=FALSE)
  d8$Fov<-7
  d8$Name<-paste(d8$Fov,d8$TRACK_ID,sep="_")
  d9 <- read.delim(paste("data/BTKO/Spots_BTKO_",el,"_08.txt",sep=""), stringsAsFactors=FALSE)
  d9$Fov<-8
  d9$Name<-paste(d9$Fov,d9$TRACK_ID,sep="_")
  
  if (el=="CTRL")
  {
    BTKO_CTRL<-rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)
    BTKO_CTRL$Case<-"BTKO_CTRL"
  } else 
  {
    if (el=="T1")
    {
      BTKO_T1<-rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)
      BTKO_T1$Case<-"BTKO_T1"
    } else
    {
      BTKO_T2<-rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)
      BTKO_T2$Case<-"BTKO_T2"
    }
  }
  
}

rm(d1,d2,d3,d4,d5,d6,d7,d8,d9)

##### read files for WT
for (el in names)
{
  print(el)
  d1 <- read.delim(paste("data/BTWT/Spots_BTWT_",el,"_00.txt",sep=""), stringsAsFactors=FALSE)
  d1$Fov<-0
  d1$Name<-paste(d1$Fov,d1$TRACK_ID,sep="_")
  d2 <- read.delim(paste("data/BTWT/Spots_BTWT_",el,"_01.txt",sep=""), stringsAsFactors=FALSE)
  d2$Fov<-1
  d2$Name<-paste(d2$Fov,d2$TRACK_ID,sep="_")
  d3 <- read.delim(paste("data/BTWT/Spots_BTWT_",el,"_02.txt",sep=""), stringsAsFactors=FALSE)
  d3$Fov<-2
  d3$Name<-paste(d3$Fov,d3$TRACK_ID,sep="_")
  d4 <- read.delim(paste("data/BTWT/Spots_BTWT_",el,"_03.txt",sep=""), stringsAsFactors=FALSE)
  d4$Fov<-3
  d4$Name<-paste(d4$Fov,d4$TRACK_ID,sep="_")
  d5<- read.delim(paste("data/BTWT/Spots_BTWT_",el,"_04.txt",sep=""), stringsAsFactors=FALSE)
  d5$Fov<-4
  d5$Name<-paste(d5$Fov,d5$TRACK_ID,sep="_")
  d6 <- read.delim(paste("data/BTWT/Spots_BTWT_",el,"_05.txt",sep=""), stringsAsFactors=FALSE)
  d6$Fov<-5
  d6$Name<-paste(d6$Fov,d6$TRACK_ID,sep="_")
  d7 <- read.delim(paste("data/BTWT/Spots_BTWT_",el,"_06.txt",sep=""), stringsAsFactors=FALSE)
  d7$Fov<-6
  d7$Name<-paste(d7$Fov,d7$TRACK_ID,sep="_")
  d8 <- read.delim(paste("data/BTWT/Spots_BTWT_",el,"_07.txt",sep=""), stringsAsFactors=FALSE)
  d8$Fov<-7
  d8$Name<-paste(d8$Fov,d8$TRACK_ID,sep="_")
  d9 <- read.delim(paste("data/BTWT/Spots_BTWT_",el,"_08.txt",sep=""), stringsAsFactors=FALSE)
  d9$Fov<-8
  d9$Name<-paste(d9$Fov,d9$TRACK_ID,sep="_")
  
  if (el=="CTRL")
  {
    BTWT_CTRL<-rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)
    BTWT_CTRL$Case<-"BTWT_CTRL"
  } else 
  {
    if (el=="T1")
    {
      BTWT_T1<-rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)
      BTWT_T1$Case<-"BTWT_T1"
    } else
    {
      BTWT_T2<-rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)
      BTWT_T2$Case<-"BTWT_T2"
    }
  }
  
}

rm(d1,d2,d3,d4,d5,d6,d7,d8,d9)

##### select only important columns
BTWT_CTRL_reduced<-BTWT_CTRL %>%
  select(TRACK_ID,POSITION_T,FRAME,MEAN_INTENSITY01,MEAN_INTENSITY02,MEAN_INTENSITY04,Fov,Case,Name)
BTWT_T2_reduced<-BTWT_T2 %>%
  select(TRACK_ID,POSITION_T,FRAME,MEAN_INTENSITY01,MEAN_INTENSITY02,MEAN_INTENSITY04,Fov,Case,Name)
BTKO_CTRL_reduced<-BTKO_CTRL %>%
  select(TRACK_ID,POSITION_T,FRAME,MEAN_INTENSITY01,MEAN_INTENSITY02,MEAN_INTENSITY04,Fov,Case,Name)
BTKO_T2_reduced<-BTKO_T2 %>% 
  select(TRACK_ID,POSITION_T,FRAME,MEAN_INTENSITY01,MEAN_INTENSITY02,MEAN_INTENSITY04,Fov,Case,Name)
BTWT_T1_reduced<-BTWT_T1 %>% 
  select(TRACK_ID,POSITION_T,FRAME,MEAN_INTENSITY01,MEAN_INTENSITY02,MEAN_INTENSITY04,Fov,Case,Name)
BTKO_T1_reduced<-BTKO_T1 %>% 
  select(TRACK_ID,POSITION_T,FRAME,MEAN_INTENSITY01,MEAN_INTENSITY02,MEAN_INTENSITY04,Fov,Case,Name)

##### remove low intensity cells, threshold is set to 200
BTWT_CTRL_reduced_ok<-remove_low_intensity(BTWT_CTRL_reduced,200)
BTWT_T1_reduced_ok<-remove_low_intensity(BTWT_T1_reduced,200)
BTWT_T2_reduced_ok<-remove_low_intensity(BTWT_T2_reduced,200)
BTKO_CTRL_reduced_ok<-remove_low_intensity(BTKO_CTRL_reduced,200)
BTKO_T1_reduced_ok<-remove_low_intensity(BTKO_T1_reduced,200)
BTKO_T2_reduced_ok<-remove_low_intensity(BTKO_T2_reduced,200)

##### sliding average
BTWT_CTRL_reduced_ok <-rolling_mean(BTWT_CTRL_reduced_ok,window=2)
BTWT_T1_reduced_ok<-rolling_mean(BTWT_T1_reduced_ok,window=2)
BTWT_T2_reduced_ok<-rolling_mean(BTWT_T2_reduced_ok,window=2)
BTKO_CTRL_reduced_ok<-rolling_mean(BTKO_CTRL_reduced_ok,window=2)
BTKO_T1_reduced_ok<-rolling_mean(BTKO_T1_reduced_ok,window=2)
BTKO_T2_reduced_ok<-rolling_mean(BTKO_T2_reduced_ok,window=2)

##### definition of phase of cells in each time point
BTKO_CTRL_color <-color_decision_morepoints_2(BTKO_CTRL_reduced_ok)
BTKO_T1_color <-color_decision_morepoints_2(BTKO_T1_reduced_ok)
BTKO_T2_color <-color_decision_morepoints_2(BTKO_T2_reduced_ok)

BTWT_CTRL_color <-color_decision_morepoints_2(BTWT_CTRL_reduced_ok)
BTWT_T1_color <-color_decision_morepoints_2(BTWT_T1_reduced_ok)
BTWT_T2_color <-color_decision_morepoints_2(BTWT_T2_reduced_ok)

##### assign the Lost label
BTKO_CTRL_color_lost<-color_lost_assign(BTKO_CTRL_color)
BTKO_T1_color_lost<-color_lost_assign(BTKO_T1_color)
BTKO_T2_color_lost<-color_lost_assign(BTKO_T2_color)

BTWT_CTRL_color_lost<-color_lost_assign(BTWT_CTRL_color)
BTWT_T1_color_lost<-color_lost_assign(BTWT_T1_color)
BTWT_T2_color_lost<-color_lost_assign(BTWT_T2_color)

##### OPTIONAL, only if a Live/Dead channel is available 
# classify "Lost" cells as dead or lost by the tracker
BTKO_CTRL_phases<-is_dead(BTKO_CTRL_color_lost)
BTKO_T1_phases<-is_dead(BTKO_T1_color_lost)
BTKO_T2_phases<-is_dead(BTKO_T2_color_lost)

BTWT_CTRL_phases<-is_dead(BTWT_CTRL_color_lost)
BTWT_T1_phases<-is_dead(BTWT_T1_color_lost)
BTWT_T2_phases<-is_dead(BTWT_T2_color_lost)

##### Analysis: story of single cells and of the complete case

# The "_phase" datasets are used in the following analysis, 
# although in some cases the Lost and Dead channels are put together as in the "_color_lost" datasets

# print profile and selected phase for each time point: 
# it's better to select only some fields of view/curves, otherwise they are too many

# print with the lost/dead information
#example
el1<-unique(BTKO_CTRL_phases$Name)[1:10]
print_images_double(BTKO_CTRL_phases %>% filter(Name %in% el1),print=TRUE,save=FALSE)

print_images_double(BTKO_T1_phases %>% filter(Name %in% el1),print=TRUE,save=FALSE)
print_images_double(BTKO_T2_phases %>% filter(Name %in% el1),print=TRUE,save=FALSE)

print_images_double(BTWT_CTRL_phases %>% filter(Name %in% el1),print=TRUE,save=FALSE)
print_images_double(BTWT_T1_phases %>% filter(Name %in% el1),print=TRUE,save=FALSE)
print_images_double(BTWT_T2_cphases %>% filter(Name %in% el1),print=TRUE,save=FALSE)

# plot all the cells from a specific condition, for more general patterns
ps1<-plot_cell_story(BTWT_CTRL_phases,"BTWT_CTRL","lost") #"death"
ps2<-plot_cell_story(BTWT_T1_phases,"BTWT_T1","lost")
ps3<-plot_cell_story(BTWT_T2_phases,"BTWT_T2","lost")
ps4<-plot_cell_story(BTKO_CTRL_phases,"BTKO_CTRL","lost")
ps5<-plot_cell_story(BTKO_T1_phases,"BTKO_T1","lost")
ps6<-plot_cell_story(BTKO_T2_phases,"BTKO_T2","lost")

(ps1|ps2|ps3)/(ps4|ps5|ps6)

# For each time frame, plot percentage of cells in each phase
BTWT_CTRL_plot<-data_for_composition_plot(BTWT_CTRL_phases)
BTKO_CTRL_plot<-data_for_composition_plot(BTKO_CTRL_phases)
BTWT_T1_plot<-data_for_composition_plot(BTWT_T1_phases)
BTKO_T1_plot<-data_for_composition_plot(BTKO_T1_phases)
BTWT_T2_plot<-data_for_composition_plot(BTWT_T2_phases)
BTKO_T2_plot<-data_for_composition_plot(BTKO_T2_phases)

data_plot_composition<-rbind(BTWT_CTRL_plot,BTKO_CTRL_plot,BTWT_T1_plot,BTKO_T1_plot,BTWT_T2_plot,BTKO_T2_plot)

data_plot_composition %>%
  #filter(Condiiton=="CTRL") %>%
  mutate(Phase=factor(color, levels=c("R","G","Y"))) %>%
  ggplot(aes(x=FRAME,y = ifelse(Type=="WT", -perc_initial, perc_initial),
             color = Phase,fill=Phase,label = Phase)) +  
  geom_area()+
  geom_hline(aes(yintercept=0))+
  annotate("text",x=400,y=60,label="KO",size=5)+
  annotate("text",x=400,y=-60,label="WT",size=5)+
  scale_color_manual(values=rcartocolor::carto_pal(n = 12, name = "Prism")[c(8,5,6)],labels=c("G1","S","M"))+
  scale_fill_manual(values=rcartocolor::carto_pal(n = 12, name = "Prism")[c(8,5,6)],labels=c("G1","S","M"))+
  facet_wrap(~Condiiton)+
  #theme_void(base_size=15)+
  scale_y_continuous(limits = c(-100,100),expand=c(0,0),
                     breaks=c(-100,-75,-50,-25,0,25,50,75,100),
                     labels=c(100,75,50,25,0,25,50,75,100))+
  scale_x_continuous(limits = c(0,531),expand=c(0,0))+
  scale_alpha_manual()+
  labs(y="Cells (%)")+
  guides(color=FALSE)+
  theme_classic(base_size=20)+
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        legend.margin=margin(t=-10))


#### Analysis of yellow phase length across condition

length_BTWT_CTRL_death<-phase_length(BTWT_CTRL_phases,"death")
length_BTWT_T1_death<-phase_length(BTWT_T1_phases,"death")
length_BTWT_T2_death<-phase_length(BTWT_T2_phases,"death")
length_BTKO_CTRL_death<-phase_length(BTKO_CTRL_phases,"death")
length_BTKO_T1_death<-phase_length(BTKO_T1_phases,"death")
length_BTKO_T2_death<-phase_length(BTKO_T2_phases,"death")

lengths_all_death<-rbind(length_BTWT_CTRL_death,length_BTWT_T1_death,length_BTWT_T2_death,
                         length_BTKO_CTRL_death,length_BTKO_T1_death,length_BTKO_T2_death)

lengths_all_death <-lengths_all_death %>% mutate(Percentage=length/total_length*100)

mycol4<-c(brewer.pal(5,"Blues")[c(2,3,4)],brewer.pal(5,"Greys")[c(2,3,4)])

lengths_all_death %>% filter(color_death=="Y") %>%
  mutate(Condition=ifelse(Case %in% c("BTWT_CTRL","BTWT_T1","BTWT_T2"),"aWT","bKO"),
         Dose=ifelse(Case %in% c("BTWT_CTRL","BTKO_CTRL"),"CTRL",ifelse(Case %in% c("BTWT_T1","BTKO_T1"),"T1","T2"))) %>%
  mutate(Dose=factor(Dose,levels=c("T2","T1","CTRL"))) %>%
  ggplot(aes(x=Dose, y=Percentage, fill=Condition)) +
  introdataviz::geom_split_violin(alpha = .9, trim = FALSE) +
  geom_boxplot(width = .2, fatten = NULL, show.legend = FALSE) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175)) +
  coord_flip()+
  scale_x_discrete(name = "", labels = c("T-DM1 1","T-DM1 0.1","Vehicle")) +
  scale_fill_manual(values=mycol4[c(4,3)],labels=c("WT","KO"))+
  labs(x=NULL,y="Cells in G2/M over total length (%)",fill="")+
  theme_classic(base_size=20)

# anova for Condition and Dose -> we can't apply anova for it is not normally distributed,
# we then applied Kruskal Wallis test
lengths_all_death_Y_anova <- lengths_all_death %>% 
  filter(color_death=="Y") %>%
  mutate(Condition=ifelse(Case %in% c("BTWT_CTRL","BTWT_T1","BTWT_T2"),"WT","KO"),
         Dose=ifelse(Case %in% c("BTWT_CTRL","BTKO_CTRL"),"CTRL",ifelse(Case %in% c("BTWT_T1","BTKO_T1"),"T1","T2"))) 

# test of significance, anova
res.aov2 <- aov(Percentage ~ Condition * Dose, 
                data = lengths_all_death_Y_anova)
summary(res.aov2)

# homogeneity of variancesfor anova
plot(res.aov2, 1)
#some outliers detected, let's see if it affects the homoegniety
leveneTest(Percentage ~ Condition * Dose, 
           data = lengths_all_death_Y_anova)
#normality
plot(res.aov2, 2)

### since the anova is not applicable, we will consider a Kruskal Wallis test
ks<-kruskal.test(Percentage ~ Case, 
                 data = lengths_all_death_Y_anova)

lengths_all_death_Y_anova %>%
  group_by(Dose) %>%
  dunn_test(Percentage ~ Condition)

#### Analysis of cells phase changing

alldata_phase<-rbind(BTWT_CTRL_phases,BTWT_T1_phases,BTWT_T2_phases,
                     BTKO_CTRL_phases,BTKO_T1_phases,BTKO_T2_phases)

# state transition t -> t+1
seqtry_2<-alldata_phase %>% 
  select(Case,FRAME, Name, color_death) %>%
  mutate(color_final=ifelse(color_death %in% c("Lost","Dead"),"Lost",color_death)) %>%
  group_by(Case,Name) %>%
  mutate(col2=lead(color_final)) %>% #find the successive color
  mutate(col2=ifelse(is.na(col2),"Lost",col2))

seqtry_3<-seqtry_2 %>% filter(!(color_final==col2))

#### graph with focus on phases

count_states<-seqtry_3 %>% group_by(Case,color_final,col2) %>%
  summarize(count=n()) %>%
  mutate(perc=round(count/sum(count) *100,2))

count_states$color_final<-case_match(count_states$color_final,"R"~"G1","G"~"S", "Y"~"M",
                                     "Lost"~"Lost")

count_states$color_final<-factor(count_states$color_final,levels=c("G1","S","M","Lost"))

cols_RYG<-c(rcartocolor::carto_pal(n = 12, name = "Prism")[c(8,5,6,2)])

count_states$Case<-factor(count_states$Case,levels=rev(c("BTWT_CTRL","BTKO_CTRL","BTWT_T1","BTKO_T1","BTWT_T2","BTKO_T2")),
                          labels=rev(c("WT, Vehicle","KO, Vehicle","WT, T-DM1 0.1","KO, T-DM1 0.1","WT, T-DM1 1","KO, T-DM1 1")))

count_states$col2<-factor(count_states$col2,levels=c("G1","S","M","Lost"))

count_states$Label<-ifelse(count_states$perc>=10,paste(round(count_states$perc,1),"%"),"")

count_states %>% 
  filter (!(color_final=="Lost")) %>% 
  ggplot(aes(x=Case,y=perc,fill=col2,alpha=col2)) +
  geom_bar(stat="identity")+
  geom_text(aes(label = Label,color=col2), 
            size = 5, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=cols_RYG)+
  scale_color_manual(values=c("black","black","black","white"))+
  scale_alpha_manual(values=c(0.8,0.8,0.8,1))+
  scale_y_continuous(limits = c(0,103),expand=c(0,0))+
  facet_wrap(~color_final)+
  labs(fill="Following phase",y="Cells (%)",x="")+
  coord_flip()+
  guides(alpha=FALSE,color=FALSE)+
  theme_classic(base_size=20)+
  theme(legend.position = "bottom")

### chi-squared test
count_states <- count_states %>% mutate("Seq"=paste(color_final,col2,sep="_"))
count_states_long<-count_states %>% filter (!(color_final=="Lost")) %>% 
  ungroup() %>%
  select(Case,Seq,count) %>% 
  spread(Case,count)

csl_m<-as.table(as.matrix(count_states_long[,-1]))
dimnames(csl_m)<-list(Seq=count_states_long$Seq,
                      Case=colnames(count_states_long[-1]))
csq<-chisq.test(csl_m)
csq
csq$stdres
