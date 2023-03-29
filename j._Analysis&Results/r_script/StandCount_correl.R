# install.packages("devtools")
#devtools::install_github("adriancorrendo/metrica")

library(metrica)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(tidyverse)
library(plyr)

rm(list=ls())

setwd("I:/My Drive/UAS_Beans/Beans_StandCount/2022/SVREC/j._Analysis&Results")
df <- read.csv('Results_pred.csv')
names(df)
str(df)

# Create list of selected metrics
selected.metrics <- c("r","MAE","RMSE", "R2")

# df2 <- df[,21:28]
# df2 <- na.omit(df2)
# str(df2)

#### Ground truth vs stand count predicted ####
# Create the plot
Boxes_gt_x_GT <- metrica::scatter_plot(data = df, 
                                               obs = StandCount_gr, pred = boxes_gt,
                                               # Activate print_metrics arg.
                                               print_metrics = TRUE, 
                                               # Indicate metrics list
                                               metrics_list = selected.metrics,
                                               # Customize metrics position
                                               position_metrics = c(x = 65 , y = 170),
                                               # Customize equation position
                                               position_eq = c(x =65, y = 130),
                                               regline_size = 1,
                                               shape_color = "steelblue") + 
  
  ylab("Annotations boxes") +
  xlab("Ground truth") +
  # Customize axis breaks
  scale_y_continuous(breaks = seq(0,300, by = 20))+
  scale_x_continuous(breaks = seq(0,300, by = 20)) #+
  #labs(title = "GT vs Pred.Drone - 7m")

Boxes_gt_x_GT

#### Annotations vs stand count predicted ####
# Create the plot
names(df)
Boxes_gt_x_pred_7m <- metrica::scatter_plot(data = df, 
                              obs = boxes_gt, pred = pred_count_01,
                              # Activate print_metrics arg.
                              print_metrics = TRUE, 
                              # Indicate metrics list
                              metrics_list = selected.metrics,
                              # Customize metrics position
                              position_metrics = c(x = 60 , y = 150),
                              # Customize equation position
                              position_eq = c(x = 60, y = 115),
                              regline_size = 1,
                              shape_color = "steelblue") + 
  
                              ylab("Pred. Drone - 7m") +
                              xlab("Annotations boxes") +
                                # Customize axis breaks
                                scale_y_continuous(breaks = seq(0,300, by = 20))+
                                scale_x_continuous(breaks = seq(0,300, by = 20)) #+
  #labs(title = "Stand count - Annotations vs predicted - 7m")

Boxes_gt_x_pred_7m


#### Annotations vs stand count predicted flight 2 ####
str(df)
# Create the plot
GT_x_pred_7m <- metrica::scatter_plot(data = df, 
                                               obs = StandCount_gr, pred = pred_count_01,
                                               # Activate print_metrics arg.
                                               print_metrics = TRUE, 
                                               # Indicate metrics list
                                               metrics_list = selected.metrics,
                                               # Customize metrics position
                                               position_metrics = c(x = 60 , y = 170),
                                               # Customize equation position
                                               position_eq = c(x = 60, y = 125),
                                               regline_size = 1,
                                               shape_color = "steelblue") + 
  
  ylab("Pred. Drone - 7m") +
  xlab("Ground truth") +
  # Customize axis breaks
  scale_y_continuous(breaks = seq(0,300, by = 20))+
  scale_x_continuous(breaks = seq(0,300, by = 20))# +
  #labs(title = "Stand count - Annotations vs predicted - 10m")

GT_x_pred_7m


### Stacked lines

df2 <- read.csv('Results_stacked1.csv')
names(df2)
str(df2)

#df2$Type <- as.factor(df2$Type)

# Color-blind safe colors
colors <- thematic::okabe_ito(8)[-4]

df_means <- ddply(df2, "Type", summarise, mean = mean(SC))
df_means

SC_lines <- df2 %>% 
  ggplot(aes(fill = Type,
             x = image_id,
             y = SC,
             color = Type)) +
  geom_line(position = "stack", size = 1.2) + 
  scale_color_manual(values = colors) + # specify it here
  theme_bw() +
  coord_cartesian(ylim = c(55, 170), expand = F) +
  xlab(NULL) + ylab(NULL) +
  theme(legend.position = "bottom", axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = seq(55,170, by = 10))+
  geom_hline(data = df_means, aes(yintercept=mean, color = Type), linetype = 2, size = 1)

SC_lines


##### Merging plots #####

ggarrange(
ggarrange(
  Boxes_gt_x_GT, GT_x_pred_7m,Boxes_gt_x_pred_7m,
  ncol = 3, nrow = 1,
  common.legend = TRUE, legend = NULL),
SC_lines, nrow = 2)


##############
##############
#### Annotations vs stand count predicted flight 10m ####
str(df)
# Create the plot
Boxes_gt_x_pred_10m <- metrica::scatter_plot(data = df, 
                                                obs = boxes_gt, pred = pred_count_10m_6_13_22,
                                                # Activate print_metrics arg.
                                                print_metrics = TRUE, 
                                                # Indicate metrics list
                                                metrics_list = selected.metrics,
                                                # Customize metrics position
                                                position_metrics = c(x = 50 , y = 150),
                                                # Customize equation position
                                                position_eq = c(x = 50, y = 110),
                                                regline_size = 1,
                                                shape_color = "steelblue") + 
  
  ylab("Pred. Drone - 10m") +
  xlab("Annotations boxes") +
  # Customize axis breaks
  scale_y_continuous(breaks = seq(0,300, by = 20))+
  scale_x_continuous(breaks = seq(0,300, by = 20)) 

Boxes_gt_x_pred_10m


#### Annotations vs stand count predicted flight early growth stage ####
str(df)
# Create the plot
Boxes_gt_x_pred_7mE <- metrica::scatter_plot(data = df, 
                                             obs = boxes_gt, pred = pred_count_7m_6_10_22,
                                             # Activate print_metrics arg.
                                             print_metrics = TRUE, 
                                             # Indicate metrics list
                                             metrics_list = selected.metrics,
                                             # Customize metrics position
                                             position_metrics = c(x = 35 , y = 150),
                                             # Customize equation position
                                             position_eq = c(x = 35, y = 100),
                                             regline_size = 1,
                                             shape_color = "steelblue") + 
  
  ylab("Pred. Drone - 7m Early") +
  xlab("Annotations boxes") +
  # Customize axis breaks
  scale_y_continuous(breaks = seq(0,300, by = 20))+
  scale_x_continuous(breaks = seq(0,300, by = 20)) 

Boxes_gt_x_pred_7mE

### Stacked lines

df3 <- read.csv('Results_stacked2.csv')
names(df3)
str(df3)

#df2$Type <- as.factor(df2$Type)

# Color-blind safe colors
colors <- thematic::okabe_ito(8)[-4]

df_means <- ddply(df3, "type", summarise, mean = mean(SC))
df_means

SC_lines2 <- df3 %>% 
  ggplot(aes(fill = type,
             x = image_id,
             y = SC,
             color = type)) +
  geom_line(position = "stack", size = 1.2) + 
  scale_color_manual(values = colors) + # specify it here
  theme_bw() +
  coord_cartesian(ylim = c(30, 160), expand = F) +
  xlab(NULL) + ylab(NULL) +
  theme(legend.position = "bottom", axis.text.x=element_blank(), 
        axis.ticks.x=element_blank() ,
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = seq(30,160, by = 10))+
  geom_hline(data = df_means, aes(yintercept=mean, color = type), linetype = 2, size = 1)

SC_lines2

########## 6 m flight ###########

df4 <- read.csv('results_6_13_SVREC_6m_TH01.csv')
names(df4)
str(df4)

# Create the plot
Boxes_gt_new_x_pred_6m <- metrica::scatter_plot(data = df4, 
                                             obs = boxes_gt, pred = pred_count,
                                             # Activate print_metrics arg.
                                             print_metrics = TRUE, 
                                             # Indicate metrics list
                                             metrics_list = selected.metrics,
                                             # Customize metrics position
                                             position_metrics = c(x = 70 , y = 150),
                                             # Customize equation position
                                             position_eq = c(x = 70, y = 115),
                                             regline_size = 1,
                                             shape_color = "steelblue") + 
  
  ylab("Pred. Drone - 6m") +
  xlab("Annotations boxes") +
  # Customize axis breaks
  scale_y_continuous(breaks = seq(0,300, by = 20))+
  scale_x_continuous(breaks = seq(0,300, by = 20)) 

Boxes_gt_new_x_pred_6m


##### Merging plots #####

ggarrange(
  ggarrange(
    Boxes_gt_x_pred_10m, Boxes_gt_x_pred_7mE, Boxes_gt_new_x_pred_6m,
    ncol = 3, nrow = 1,
    common.legend = TRUE, legend = NULL),
  SC_lines2, nrow = 2) 



#### Repetitions correlations- Using BLUPs ####
# 
# library(metan)
# names(df)
# #df2 <- df[-c(13:25)] %>% as.data.frame()
# df3 <- df %>% 
#   select(Name, REP, IBLK, Experiment, StandCount_gt)
# str(df3)
# 
# df3$Name <- as.factor(df3$Name)
# df3$REP <- as.factor(df3$REP)
# df3$IBLK <- as.factor(df3$IBLK)
# df3$Experiment <- as.factor(df3$Experiment)
# str(df3)
# 
# df3 <- na.omit(df3)
# mod1 <- gamem(df3,
#                gen = Name,
#                rep = REP,
#                block = IBLK,
#                resp = StandCount_gt,
#               by = Experiment)
# 
# get_model_data(mod1, "details") 
# # Class of the model: gamem
# # Variable extracted: details
# get_model_data(mod1, "lrt") 
# # Class of the model: gamem
# # Variable extracted: lrt
# get_model_data(mod1, "genpar")
# # Class of the model: gamem
# # Variable extracted: genpar
# data <- get_model_data(mod1)
# 
# dfrep1<- df3 %>% 
#   subset(df3$REP == 1) %>% 
#   arrange(Name)
# 
# dfrep2<- df3 %>% 
#   subset(df3$REP == 2) %>% 
#   arrange(Name)
# 
# dfrep <- cbind(dfrep1,dfrep2)
# str(dfrep)
# colnames(dfrep)<- c("Name",            "REP"  ,           "IBLK"   ,         "Experiment", "StandCount_gt" ,         
#                      "Name2",            "REP2"  ,           "IBLK2"  ,          "Experiment2" ,"StandCount_gt2"  )
# ### Ploting the Reps
# plot_canopy_mask_VI <- metrica::scatter_plot(data = dfrep, 
#                                              obs = StandCount_gt, pred = StandCount_gt2,
#                                              # Activate print_metrics arg.
#                                              print_metrics = T, 
#                                              # Indicate metrics list
#                                              metrics_list = selected.metrics,
#                                              # Customize metrics position
#                                              position_metrics = c(x = 140 , y = 100),
#                                              # Customize equation position
#                                              #print_eq = F,
#                                              position_eq = c(x = 140, y = 110),
#                                              regline_size = 1,
#                                              shape_color = "steelblue") + 
#   
#   ylab("REP 2") +
#   xlab("REP 1") +
#   # Customize axis breaks
#   scale_y_continuous(breaks = seq(0,300, by = 20))+
#   scale_x_continuous(breaks = seq(0,300, by = 20)) +
#   labs(title = "Correlation between experiement reps")
# 
# 
# plot_canopy_mask_VI



















#### Correlation table ####

names(df)
selected.metrics <- c("r","MAE","RMSE", "R2")

metrics.sum1 <- df %>% 
  metrics_summary( obs = StandCount_gr, pred = boxes_gt,
                   type = "regression")  %>% 
  filter(Metric %in% c("r","MAE","RMSE", "R2")) %>% 
  mutate(Method = c("Boxes_gt_x_GT"))
  
metrics.sum2 <- df %>% 
  metrics_summary( obs = boxes_gt, pred = pred_count_01,
                   type = "regression")  %>% 
  filter(Metric %in% c("r","MAE","RMSE", "R2")) %>% 
  mutate(Method = c("Boxes_gt_x_pred_count")) 

metrics.sum3 <- df %>% 
  metrics_summary( obs = boxes_gt, pred = pred_count_10m_6_13_22,
                   type = "regression")  %>% 
  filter(Metric %in% c("r","MAE","RMSE", "R2")) %>% 
  mutate(Method = c("Boxes_gt_x_pred_count2"))

metrics.sum4 <- df %>% 
  metrics_summary( obs = boxes_gt, pred = pred_count_7m_6_10_22,
                   type = "regression")  %>% 
  filter(Metric %in% c("r","MAE","RMSE", "R2")) %>% 
  mutate(Method = c("Boxes_gt_x_pred_count3"))


data_correl_final<- rbind(metrics.sum1, metrics.sum2, metrics.sum3,
                          metrics.sum4
                          )

#write.csv(data_correl_final, "data_correl_final.csv")



