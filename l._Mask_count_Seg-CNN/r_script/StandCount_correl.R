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

setwd("I:/My Drive/UAS_Beans/Beans_StandCount/2022/SVREC/l._Mask_count_Seg-CNN/Results")
df <- read.csv('Results_pred.csv')
names(df)
str(df)


# Create list of selected metrics
selected.metrics <- c("r","MAE","RMSE", "R2")

#### Ground truth vs stand count predicted ####
# Create the plot
# Boxes_gt_x_GT <- metrica::scatter_plot(data = df, 
#                                                obs = StandCount_gr, pred = boxes_gt,
#                                                # Activate print_metrics arg.
#                                                print_metrics = TRUE, 
#                                                # Indicate metrics list
#                                                metrics_list = selected.metrics,
#                                                # Customize metrics position
#                                                position_metrics = c(x = 65 , y = 170),
#                                                # Customize equation position
#                                                position_eq = c(x =65, y = 130),
#                                                regline_size = 1,
#                                                shape_color = "steelblue") + 
#   
#   ylab("Annotations boxes") +
#   xlab("Ground truth") +
#   # Customize axis breaks
#   scale_y_continuous(breaks = seq(0,300, by = 20))+
#   scale_x_continuous(breaks = seq(0,300, by = 20)) #+
#   #labs(title = "GT vs Pred.Drone - 7m")
# 
# Boxes_gt_x_GT


#### Correlation table1 ####

names(df)

metrics.sum1 <- df %>% 
  metrics_summary( obs = boxes_gt, pred = pred_count_01,
                   type = "regression")  %>% 
  filter(Metric %in% c("r","MAE","RMSE", "R2")) %>% 
  mutate(Method = c("Boxes_gt_x_pred_CNN"))
  
metrics.sum2 <- df %>% 
  metrics_summary( obs = boxes_gt, pred = SC_7m_OpenCV1,
                   type = "regression")  %>% 
  filter(Metric %in% c("r","MAE","RMSE", "R2")) %>% 
  mutate(Method = c("Boxes_gt_x_OpenCV1")) 

metrics.sum3 <- df %>% 
  metrics_summary( obs = boxes_gt, pred = SC_7m_OpenCV2,
                   type = "regression")  %>% 
  filter(Metric %in% c("r","MAE","RMSE", "R2")) %>% 
  mutate(Method = c("Boxes_gt_x_OpenCV2"))

metrics.sum4 <- df %>% 
  metrics_summary( obs = boxes_gt, pred = SC_7m_WS,
                   type = "regression")  %>% 
  filter(Metric %in% c("r","MAE","RMSE", "R2")) %>% 
  mutate(Method = c("Boxes_gt_x_WS"))


data_correl_final<- rbind(metrics.sum1, metrics.sum2, metrics.sum3,
                          metrics.sum4
                          )
data_correl_final
write.csv(data_correl_final, "data_correl_CNN_Trad.csv")


#### Correlation table2 ####

names(df)

metrics.sum1 <- df %>% 
  metrics_summary( obs = StandCount_gr, pred = pred_count_01,
                   type = "regression")  %>% 
  filter(Metric %in% c("r","MAE","RMSE", "R2")) %>% 
  mutate(Method = c("GT_x_pred_CNN"))

metrics.sum2 <- df %>% 
  metrics_summary( obs = StandCount_gr, pred = SC_7m_OpenCV1,
                   type = "regression")  %>% 
  filter(Metric %in% c("r","MAE","RMSE", "R2")) %>% 
  mutate(Method = c("GT_x_OpenCV1")) 

metrics.sum3 <- df %>% 
  metrics_summary( obs = StandCount_gr, pred = SC_7m_OpenCV2,
                   type = "regression")  %>% 
  filter(Metric %in% c("r","MAE","RMSE", "R2")) %>% 
  mutate(Method = c("GTt_x_OpenCV2"))

metrics.sum4 <- df %>% 
  metrics_summary( obs = StandCount_gr, pred = SC_7m_WS,
                   type = "regression")  %>% 
  filter(Metric %in% c("r","MAE","RMSE", "R2")) %>% 
  mutate(Method = c("GT_x_WS"))


data_correl_final<- rbind(metrics.sum1, metrics.sum2, metrics.sum3,
                          metrics.sum4
)
data_correl_final
write.csv(data_correl_final, "data_correl_CNN_Trad2.csv")




