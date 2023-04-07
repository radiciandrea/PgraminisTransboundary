rm(list = ls())

library(sf)
library(Matrix)
library(dplyr)
library(ggplot2)
library(pracma)
library(ggplot2)

i_c <- 1:n

load("Data/F_C_y.RData")

B_C_y <- lapply(X = F_C_y[1:4], FUN = function(x){return(1*(x>0))}) #first 4 years only: 2013:2016
B_C_ys <- Reduce('+', B_C_y)
B_C_i_c = 1*(B_C_ys >=3) #equivalent to >= 75%
diag(B_C_i_c) <- 1 #this is the "C_D"

F_C_i_v <- Reduce('pmin', F_C_y[5:6]) #last two years 2017:2018
B_C_i_v = 1*(F_C_i_v > 0)
diag(B_C_i_v) = 1 #this is the "C_V"

save(B_C_i_c, B_C_i_v , file = "Data/B_C_i_calval.RData")  #We save C_D and C_V
