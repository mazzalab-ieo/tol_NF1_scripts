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

setwd("~/tol_NF1_scripts/FUCCI")

source('scripts/remove_low_intensity.R')
source('scripts/rolling_mean.R')
source('scripts/color_decision_morepoints_2.R')
source('scripts/color_lost_assign.R')
source('scripts/is_dead.R')
source('scripts/print_images_double.R')
source('scripts/plot_cell_story.R')
source('scripts/data_for_composition_plot.R')

##### read files for KO
# only field of view n. 0 
names<-c("CTRL","T1","T2")

for (el in names)
{
  print(el)
  d1 <- read.csv2(paste("~/tol_NF1_scripts/FUCCI/data/Spots_BTKO_",el,"_00.txt",sep=""), stringsAsFactors=FALSE)
  d1$Fov<-0
  d1$Name<-paste(d1$Fov,d1$TRACK_ID,sep="_")

  if (el=="CTRL")
  {
    BTKO_CTRL<-d1
    BTKO_CTRL$Case<-"BTKO_CTRL"
  } else 
  {
    if (el=="T1")
    {
      BTKO_T1<-d1
      BTKO_T1$Case<-"BTKO_T1"
    } else
    {
      BTKO_T2<-d1
      BTKO_T2$Case<-"BTKO_T2"
    }
  }
  
}

rm(d1)

##### read files for WT
# only field of view n. 0 

for (el in names)
{
  print(el)
  d1 <- read.csv2(paste("~/tol_NF1_scripts/FUCCI/data/Spots_BTWT_",el,"_00.txt",sep=""), stringsAsFactors=FALSE)
  d1$Fov<-0
  d1$Name<-paste(d1$Fov,d1$TRACK_ID,sep="_")
  
  if (el=="CTRL")
  {
    BTWT_CTRL<-d1
    BTWT_CTRL$Case<-"BTWT_CTRL"
  } else 
  {
    if (el=="T1")
    {
      BTWT_T1<-d1
      BTWT_T1$Case<-"BTWT_T1"
    } else
    {
      BTWT_T2<-d1
      BTWT_T2$Case<-"BTWT_T2"
    }
  }
  
}

rm(d1)

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
#example: el1<-unique(BTKO_CTRL_phases$Name)[1:10]
print_images_double(BTKO_CTRL_color_lost,print=TRUE,save=FALSE)
print_images_double(BTKO_T1_color_lost,print=TRUE,save=FALSE)
print_images_double(BTKO_T2_color_lost,print=TRUE,save=FALSE)

print_images_double(BTWT_CTRL_color_lost,print=TRUE,save=FALSE)
print_images_double(BTWT_T1_color_lost,print=TRUE,save=FALSE)
print_images_double(BTWT_T2_color_lost,print=TRUE,save=FALSE)

# plot all the cells from a specific condition, for more general patterns
# this is useful when more cells are analysed

ps1<-plot_cell_story(BTWT_CTRL_phases,"BTWT_CTRL","lost") #"death"
ps2<-plot_cell_story(BTWT_T1_phases,"BTWT_T1","lost")
ps3<-plot_cell_story(BTWT_T2_phases,"BTWT_T2","lost")
ps4<-plot_cell_story(BTKO_CTRL_phases,"BTKO_CTRL","lost")
ps5<-plot_cell_story(BTKO_T1_phases,"BTKO_T1","lost")
ps6<-plot_cell_story(BTKO_T2_phases,"BTKO_T2","lost")

(ps1|ps2|ps3)/(ps4|ps5|ps6)

# For each time frame, plot percentage of cells in each phase
# this is useful when more cells are analysed

BTWT_CTRL_plot<-data_for_composition_plot(BTWT_CTRL_phases)
BTKO_CTRL_plot<-data_for_composition_plot(BTKO_CTRL_phases)
BTWT_T1_plot<-data_for_composition_plot(BTWT_T1_phases)
BTKO_T1_plot<-data_for_composition_plot(BTKO_T1_phases)
BTWT_T2_plot<-data_for_composition_plot(BTWT_T2_phases)
BTKO_T2_plot<-data_for_composition_plot(BTKO_T2_phases)

data_plot_composition<-rbind(BTWT_CTRL_plot,BTKO_CTRL_plot,BTWT_T1_plot,BTKO_T1_plot,BTWT_T2_plot,BTKO_T2_plot)

data_plot_composition %>%
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
