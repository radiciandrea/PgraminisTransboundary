rm(list = ls())

setwd("C:/Users/radya/Desktop/Alcuni file permanenti/Dottorato/ArticoliConferenze/08_Puccinia_2_transboundary/Codice_da_pulire")

library(sf)
library(Matrix)
library(dplyr)
library(pracma)
library(ggplot2)
library(purrr)
library(reshape2)
library(scales)
library(RColorBrewer)
library(distillery)

load("Data/alpha_df_cal.RData")
load("Data/alpha_df_val.RData")

alpha_df <- data.frame(Country = alpha_df_cal$Country,
                      Continent = alpha_df_cal$Continent,
                      Alpha_cal = alpha_df_cal$mean_alpha,
                      Alpha_val = alpha_df_val$mean_alpha)

# fig. S5
ggplot(alpha_df, aes(x=Alpha_cal, y= Alpha_val, color = Continent)) + 
  geom_hline(yintercept=1, color = "gray")+
  geom_vline(xintercept=1, color = "gray")+
  geom_point(size=1.5) + 
  geom_smooth(data = alpha_df, aes(x = Alpha_cal, y = Alpha_val), method=lm, se=FALSE, color = "black") +
  xlab("Average alpha (design)") +
  ylab("Average alpha (validation)") + 
  theme_test()
